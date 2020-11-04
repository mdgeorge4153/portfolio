open Async.Std

exception Out_of_bounds

type entry = {
  path : bytes;
  length : int64;
}

type file = {
  spec : entry;
  reader : Reader.t;
  writer : Writer.t;
  fd : Fd.t;
}

type t = {
  files : file list;
  length : int64;
}

(* 6 - user read/write
   4 - group read
   4 - world read *)
let create_perm = 0o644

let partition_map ls ~f =
  ls
  |> List.map f
  |> List.partition (function
      | `Left _ -> true
      | `Right _ -> false)
  |> fun (l, r) ->
  List.map (function `Left x -> x | `Right _ -> failwith "Invalid state.") l,
  List.map (function `Right x -> x | `Left _ -> failwith "Invalid state.") r

let make ~(op : [`Create | `Load]) entries =
  entries
  |> List.map (fun ({path} as entry) ->
      let open Unix in
      (* Rdrw (RDWR) opens the file for reading and writing.
         Creat (CREAT) and Excl (EXCL) together throw an error if the file exists. *)
      Monitor.try_with (fun () ->
          openfile
            ~perm:create_perm
            ~mode:(`Rdwr :: if op = `Create then [`Creat; `Excl] else [])
            path)
      >>= function
      | Core.Std.Result.Ok fd -> return (`Ok (entry, fd))
      | Core.Std.Result.Error _ -> return (`Error entry))
  |> Deferred.all
  >>= fun efds ->
  let oks, errs = partition_map ~f:(function `Ok x -> `Left x | `Error {path} -> `Right path) efds in
  match oks, errs with
  | _, (_ :: _ as errs) ->
    let errs' = String.concat ", " errs in
    begin match op with
      | `Create ->
        (* We want to restore the original state.
           Delete the empty files that we created. *)
        oks
        |> List.map (fun ({path}, _) ->
            Unix.unlink path)
        |> Deferred.all
        >>= fun _ ->
        failwith (Printf.sprintf "create: The following files already exist in the filesystem: %s." errs')
      | `Load ->
        failwith (Printf.sprintf "load: The following files did not exist in the filesystem: %s" errs')
    end
  | _, [] ->
    (* Successfully created or loaded all files, none existed.
       First, if we are creating, fill the files with zeroes so they match the length in the spec.
       If we are loading, make sure the sizes are correct. *)
    oks
    |> List.map (fun ({path; length}, fd) ->
        match op with
        | `Create ->
          Unix.truncate path ~len:length
        | `Load ->
          Unix.fstat fd
          >>= fun {Unix.Stats.size} ->
          if size = length then return ()
          else failwith (Printf.sprintf "load: The file %s has size %Ld, but we expected it to have size %Ld." path size length))
    |> Deferred.all
    (* Now create the [file]s to put in the [t]. *)
    >>= fun _ ->
    List.map (fun (entry, fd) ->
        {spec=entry;
         reader=Reader.create fd;
         writer=Writer.create fd;
         fd}) oks
    |> return
    >>= fun files ->
    return {files;
            length=List.fold_left (fun a {spec={length}} -> Int64.add a length) Int64.zero files}

let create = make ~op:`Create

let load = make ~op:`Load

let length {length} = length

let which {files} pos len =
  let rec find_start files p =
    match files with
    | [] -> None
    | {spec={length}} :: fs ->
      if p < length then Some (files, p)
      else find_start fs (Int64.sub p length)
  in
  let rec distribute files p off rem =
    match files, rem with
    | _, i when i = Int64.zero -> []
    | [], _ -> raise Out_of_bounds
    | {spec={length}} as f :: fs, _ ->
      let l = min (Int64.sub length p) rem in
      (* off will be the offset into the input string
         p will be the offset into the file
         l will be the length from [off] in the input string to the end of the
         section to be distributed to this file *)
      (f, off, p, l) :: distribute fs Int64.zero (Int64.add off l) (Int64.sub rem l)
  in
  match find_start files pos with
  | None -> None
  | Some (files, p) ->
    match distribute files p Int64.zero len with
    | xs -> Some xs
    | exception Out_of_bounds -> None

let write fs ~pos s =
  let write' ({writer; fd}, off, p, l) =
    Unix.lseek fd p ~mode:`Set
    >>= fun _ ->
    (* NB: [pos] is an offset into the buffer, *not* into the file. That's why
       we call [lseek]. *)
    let () =  Writer.write ~pos:(Int64.to_int off) ~len:(Int64.to_int l) writer s in
    Writer.flushed writer
  in
  match which fs pos (Int64.of_int (String.length s)) with
  | None -> raise Out_of_bounds
  | Some ws ->
    List.map write' ws
    |> Deferred.all
    >>= fun _ ->
    return ()

let read fs ~pos ~len =
  let out = Buffer.create len in
  let read' ({spec={path}; reader; fd}, off, p, l) =
    let buf = Bytes.make (Int64.to_int l) ' ' in
    Unix.lseek fd p ~mode:`Set
    >>= fun _ ->
    Reader.really_read reader ~pos:0 ~len:(Int64.to_int l) buf
    >>= function
    | `Ok ->
      Buffer.add_bytes out buf ;
      return ()
    | `Eof _ -> failwith (Printf.sprintf "Unexpected EOF while reading '%s'." path)
  in
  let () = if len < 0 || pos < 0 then invalid_arg "[len] and [pos] must be nonnegative." in
  match which fs (Int64.of_int pos) (Int64.of_int len) with
  | None -> raise Out_of_bounds
  | Some ws ->
    List.fold_left (fun a l -> a >>= fun () -> read' l) (return ()) ws
    >>= fun () ->
    return (Buffer.to_bytes out)
