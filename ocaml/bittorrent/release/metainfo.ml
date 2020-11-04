open Async.Std
open Core.Std
open BExpr
open SHA1

(** type definitions, see .mli *)

type filename = Bytes.t

type peer_id = SHA1.t

type t = {
  announce     : Uri.t;
  name         : Bytes.t;
  piece_length : int;
  piece_hashes : SHA1.t array;
  files        : FileSet.entry list;
  peer_id      : peer_id;
  hash         : SHA1.t;
}

let rec make_piece_hashes pieces total =
  let hashes' = Array.create ~len:total (SHA1.hash "") in
  let hashes = Array.to_array hashes' in
  let rec hash pieces n =
    if n >= total then
      hashes
    else
      begin
        hashes.(n) <- SHA1.of_bytes (String.sub pieces (n * 20) 20);
        hash pieces (n + 1)
      end in
  hash pieces 0

let parse_multi_file_dict name files =
  let rec parse_file_lst lst acc =
    match lst with
    | (BDict a_lst)::t ->
      begin
        let path = match List.Assoc.find a_lst "path" with
                   | Some (BList l) -> List.fold_left l ~f:(fun a e -> a ^ (encode e)) ~init:name
                   | _              -> failwith "invalid bencoding" in
        let length = match List.Assoc.find a_lst "length" with
                     | Some (BInt l) -> l
                     | _             -> failwith "invalid bencoding" in
        parse_file_lst t ({FileSet.path = path; FileSet.length = length}::acc)
      end
    | []               -> List.sort
                           (fun {FileSet.length = _; FileSet.path = a}
                              {FileSet.length = _; FileSet.path = b} ->
                             compare a b) acc
    | _                -> failwith "invalid bencoding" in
  parse_file_lst files []

let make_metainfo url lst hash =
  let m_announce = url in
  let m_name = match List.Assoc.find lst "name" with
               | Some (BString n) -> n
               | _                -> failwith "invalid bencoding" in
  let m_piece_length = match List.Assoc.find lst "piece length" with
                       | Some (BInt l) -> Int64.to_int_exn l
                       | _      -> failwith "invalid bencoding" in
  let m_piece_hashes = match List.Assoc.find lst "pieces" with
                       | Some (BString s) when (Bytes.length s) % 20 = 0 ->
                         make_piece_hashes s ((Bytes.length s) / 20)
                       | _                                               ->
                         failwith "invalid bencoding" in
  let m_files = match List.Assoc.find lst "files", List.Assoc.find lst "length" with
                | Some (BList l), None -> parse_multi_file_dict m_name l
                | None, Some (BInt l)  -> [{FileSet.path = m_name; FileSet.length = l}]
                | _                    -> failwith "invalid bencoding" in
  let m_peer_id = Random.self_init (); SHA1.hash (string_of_int (Random.bits ())) in
  let m_hash = hash in
  {announce = m_announce; name = m_name; piece_length = m_piece_length;
    piece_hashes = m_piece_hashes; files = m_files; peer_id = m_peer_id;
      hash = m_hash}

let parse_torrent_file s =
  try_with
    (fun () ->
      Reader.with_file s ~f:decode >>= (function
      | `Success b -> return b
      | `Failure   -> failwith "could not parse torrent file") >>=
      fun b_dict ->
        let l = match b_dict with
        | BDict lst -> lst
        | _         -> failwith "invalid bencoding" in
        let announce = match List.Assoc.find l "announce" with
                       | Some (BString url) -> Uri.of_string url
                       | _                  -> failwith "invalid bencoding" in
        let info, hash = match List.Assoc.find l "info" with
                         | Some (BDict lst) -> lst, SHA1.hash (encode (BDict lst))
                         | _                -> failwith "invalid bencoding" in
        return (announce, info, hash) >>=
      fun (u, l, h) -> return (`Success (make_metainfo u l h))) >>= function
        | Ok x  -> return x
        | Error _ -> return `Failed

