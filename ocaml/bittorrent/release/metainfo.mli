open Async.Std
open SHA1

type peer_id = SHA1.t

type filename = bytes

type t = {
  (** the url of the tracker *)
  announce     : Uri.t;

  (** the name of the file *)
  name         : bytes;

  (** the length of the file pieces; typically 2^18 = 256k *)
  piece_length : int;

  (** the SHA1 hash of each piece *)
  piece_hashes : SHA1.t array;

  (** the file or set of files to download into *)
  files        : FileSet.entry list;

  (** the id of the local peer *)
  peer_id      : peer_id;

  (** the SHA1 hash of the "info" part of the metainfo file *)
  hash         : SHA1.t;
}

(** read a .torrent file and return the corresponding Metainfo.t *)
val parse_torrent_file : filename -> [`Failed | `Success of t] Deferred.t
