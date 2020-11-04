open Async.Std

type inet_address = Core.Std.Unix.Inet_addr.t * int

module Tracker : sig

  type request = {
    (** the hash of the info part of the metainfo file for the torrent *)
    info_hash : SHA1.t;

    (** a string of length 20 that identifies the requesting peer *)
    peer_id   : Metainfo.peer_id;

    (** the address of the requesting peer *)
    address : inet_address;

    (** the number of bytes that the requesting peer still needs to download *)
    left : int64;

    (**
     * the reason for the request:
     *    - `Regular indicates that this is a periodic request to the tracker
     *    - `Started indicates that the peer is just starting a download
     *    - `Completed indicates that the peer has just completed downloading
     *      the file
     *    - `Stopped indicates that the peer has stopped downloading
     *)
    event : [`Regular | `Started | `Completed | `Stopped];
  }

  type response = {
    (** the number of seconds the downloader should wait before regular rerequests *)
    interval : int;

    (** a lit of the addresses of peers *)
    peers : inet_address list;
  }
end

module Peer : sig
  type message =
    | Keepalive
    | Choke
    | Unchoke
    | Interested
    | Uninterested
    | Have     of BlockSet.piece_id
    | Bitfield of Bitfield.readonly Bitfield.t
    | Request  of BlockSet.block_id
    | Cancel   of BlockSet.block_id
    | Piece    of BlockSet.block_id * bytes
end

(** takes the metainfo to use for the connection and the *local* address *)
val do_tracker_request :
  Metainfo.t   ->
  Tracker.request ->
  [`Failed | `Success of Tracker.response] Deferred.t

type peer_connection =
  Metainfo.peer_id * Peer.message Pipe.Reader.t * Peer.message Pipe.Writer.t

val connect_to_peer :
  Metainfo.t       ->
  inet_address     ->
  [`Failed | `Success of peer_connection] Deferred.t

val listen_for_peers :
  Metainfo.t ->
  [`Failed | `Success of inet_address * peer_connection Pipe.Reader.t] Deferred.t

