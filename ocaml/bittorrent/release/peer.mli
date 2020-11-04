open Async.Std

type t

val create :
  Metainfo.t ->
  BlockSet.t ->
  Protocol.Peer.message Pipe.Reader.t ->
  Protocol.Peer.message Pipe.Writer.t ->
  t


val id              : t -> Metainfo.peer_id

(** Returns true if the peer wishes to download from us *)
val peer_interested : t -> bool

(** Returns true if we are being choked by the peer *)
val peer_choking    : t -> bool

(** Returns the Bitfield that Peer has *)
val peer_contents   : t -> Bitfield.readonly Bitfield.t

(** Returns true if the peer is being choked by us *)
val we're_choking   : t -> bool

(** Returns an option telling whether or not the peer has been updated *)
val updated         : t -> t option Deferred.t

(** Attempts to fetch the given chunk.  Returns None if the peer fails or chokes
    or the request is cancelled (by calling cancel) before the chunk can be
    fetched.  *)
val get    : t -> BlockSet.block_id -> bytes option Deferred.t

(** Cancels the given request to the remote peer *)
val cancel : t -> BlockSet.block_id -> unit

(** Prevents the peer from downloading from us *)
val choke   : t -> unit

(** Allows the peer to download from us *)
val unchoke : t -> unit

