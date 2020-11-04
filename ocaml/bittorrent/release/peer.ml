open Async.Std

(*Replace this *)
type t = unit

let create :
  Metainfo.t ->
  BlockSet.t ->
  Protocol.Peer.message Pipe.Reader.t ->
  Protocol.Peer.message Pipe.Writer.t ->
  t
  = failwith "TODO"


let id              : t -> Metainfo.peer_id = failwith "TODO"
let peer_interested : t -> bool = failwith "TODO"
let peer_choking    : t -> bool = failwith "TODO"
let peer_contents   : t -> Bitfield.readonly Bitfield.t = failwith "TODO"
let we're_choking   : t -> bool = failwith "TODO"

let updated         : t -> t option Deferred.t = failwith "TODO"

let get    : t -> BlockSet.block_id -> bytes option Deferred.t = failwith "TODO"

let cancel : t -> BlockSet.block_id -> unit = failwith "TODO"

let choke   : t -> unit = failwith "TODO"

let unchoke : t -> unit = failwith "TODO"