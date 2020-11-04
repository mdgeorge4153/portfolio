open Async.Std

(******************************************************************************)
(** type definitions, see .mli ************************************************)
(******************************************************************************)

type piece_id = int

type block_id = {
  piece  : piece_id;
  offset : int;
  length : int;
}

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(**
 * return the list [start; start+step; start+2step; ...] up to
 * but not including end.
 *)
let rec range ?(start=0) ?(step=1) n =
  if (step > 0 && start < n) || (step > 0 && start > n)
  then
    start :: (range ~start:(start+1) ~step n)
  else
    []

(******************************************************************************)
(** internal type definitions *************************************************)
(******************************************************************************)

module Piece = struct
  module State = struct
    type t =
      | Unverified
      | Verified
      | Empty
      | Progress of Bitfield.writeable Bitfield.t
  end

  type t = {
    index  : piece_id;
    hash   : SHA1.t;
    has    : unit Ivar.t;
    length : int;

    mutable status : State.t;
  }
end


type t = {
  metainfo : Metainfo.t;
  fileset  : FileSet.t;
  pieces   : Piece.t array;

  mutable need : piece_id list;

  failures : piece_id AQueue.t;
}

(******************************************************************************)
(** public interface **********************************************************)
(******************************************************************************)

let fileset          {fileset}  = fileset
let metainfo         {metainfo} = metainfo
let failed_to_verify {failures} = failures
let need             {need}     = need

let num_pieces bs =
  Array.length bs.pieces

let all_pieces bs =
  range (num_pieces bs)

let contents bs : Bitfield.readonly Bitfield.t =
  Array.fold_left (fun a {Piece.index; status} ->
      match status with
      | Piece.State.Verified ->
        Bitfield.set a index
      | _ -> a) (Bitfield.create_empty (num_pieces bs)) bs.pieces

let has bs id =
  Ivar.read bs.pieces.(id).Piece.has

let write bs {piece=id; offset; length} data =
  let open Piece in
  let open Piece.State in
  let piece = bs.pieces.(id) in
  let () = match piece.status with
    | Verified -> invalid_arg (Printf.sprintf "The piece to which the provided block belongs (%d) has already been verified and cannot be written to." id)
    | _ -> ()
  in
  let piece_length = bs.metainfo.Metainfo.piece_length in
  let () = if length <> Bytes.length data then invalid_arg "[length] must equal the length of [data]." else () in
  let () = if offset + length > piece_length then invalid_arg "[offset + length] must be less than or equal to [piece_length]." else () in
  let pos = Int64.of_int (id * piece_length + offset) in
  FileSet.write bs.fileset ~pos data
  >>= fun () ->
  let () =
    (* Update status to Progress if Empty. *)
    match piece.status with
      | Empty ->
        piece.status <- Progress (Bitfield.create_empty piece_length) ;
      | _ -> () ;
    (* Update which bytes we have read. *)
    match piece.status with
      | Progress field ->
        for i = offset to offset + length do
          Bitfield.imp_set field i
        done
      | _ -> ()
  in
  (* Verify if all bits read. *)
  let verify = match piece.status with
    | Progress field -> Bitfield.all_true field
    | Unverified -> true
    | Empty -> false
    | Verified -> failwith "Illegal state: piece should not be verified!"
  in
  if verify then
    FileSet.read bs.fileset ~pos:(id * piece_length) ~len:piece_length
    >>= fun contents ->
    if SHA1.hash contents = piece.hash then begin
      piece.status <- Verified ;
      Ivar.fill piece.has () ;
      bs.need <- List.filter (fun id' -> id <> id') bs.need
    end else begin
      AQueue.enqueue bs.failures id ;
      piece.status <- Unverified
    end ;
    return ()
  else return ()

let read bs {piece=id; offset; length} =
  let piece_length = bs.metainfo.Metainfo.piece_length in
  let () = if offset + length > piece_length then invalid_arg "[offset + length] must be less than or equal to [piece_length]." else () in
  let pos = id * piece_length + offset in
  FileSet.read bs.fileset ~pos ~len:length


let bytes_left ({metainfo} as bs) =
  let len  = Int64.of_int (metainfo.Metainfo.piece_length) in
  let need = Int64.of_int (List.length (need bs)) in
  Int64.mul len need

(* https://wiki.theory.org/BitTorrentSpecification says 16KB blocks are a good
   idea. *)
let block_size = 16 lsl 10
let blocks_of_piece {metainfo={Metainfo.piece_length}} id =
  let offsets = range ~init:start ~step:block_size piece_length in
  let result  = List.map (fun off -> {piece: offsets
  unfold_right
    (( <= ) piece_length)
    (fun p -> {piece=id; offset=start + p; length=min block_size (piece_length - p)})
    (( + ) block_size)
    0

(******************************************************************************)
(** BlockSet creation *********************************************************)
(******************************************************************************)

let create metainfo fileset =
  let open Metainfo in
  let open Core.Std.Int64 in

  FileSet.load metainfo.entries >>= fun (status,fileset) ->

  let create_piece i = {
    Piece.index = i;
    hash        = metainfo.piece_hashes.(i);
    has         = Ivar.create ();
    status      = Piece.State.Empty;
    length      = if i = FileSet.length files then
  } in

  let result = {
    metainfo;
    fileset;
    pieces   = Array.init num_pieces create_piece;
    need     = range num_pieces;
    failures = AQueue.make ()
  } in

  if status = `Created then
    return result
  else
    Deferred.List.iter (need result) begin fun piece ->
      Deferred.List.iter (blocks_of_piece result piece) begin fun block_id ->
        write result 
    

