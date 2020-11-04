open Async.Std

type t

val create : Metainfo.t -> t Deferred.t

(** Returns a Pipe that acts as a queue to all Peers *)
val get_all_peers      : t -> Peer.t Pipe.Reader.t

(** Returns a list of all of the peers currently connected to *)
val get_current_peers  : t -> Peer.t list

(** Returns the metainfo associated with the Session *)
val get_metainfo       : t -> Metainfo.t

(** Returns the block set associated with the Session *)
val get_blockset       : t -> BlockSet.t
