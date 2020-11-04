open Async.Std
open Metainfo
open Protocol.Tracker

type t = {
  current_peers : (peer_id, Peer.t) Hashtbl.t;
  all_peers_r   : Peer.t Pipe.Reader.t;
  all_peers_w   : Peer.t Pipe.Writer.t;

  meta_info  : Metainfo.t;
  my_addr    : Protocol.inet_address;

  blocks     : BlockSet.t;
}

type session = t

let add_connection (t : session) ((id, peer_in, peer_out) : Protocol.peer_connection) =
  printf "adding peer connection\n";
  if not (Hashtbl.mem t.current_peers id) then
    let peer = Peer.create t.meta_info t.blocks peer_in peer_out in
    Hashtbl.add t.current_peers id peer;
    printf "adding peer to all_peers\n";
    Pipe.write_without_pushback t.all_peers_w peer;
    upon (Peer.updated peer) begin fun update ->
      Hashtbl.remove t.current_peers id;
      match update with
	| Some peer' -> Hashtbl.add t.current_peers id peer'
	| None       -> ()
    end
  else
    Pipe.close_read peer_in;
    Pipe.close peer_out

let rec interact_with_tracker (session : t) reason () =
  let request = {
    info_hash  = session.meta_info.hash;
    peer_id    = session.meta_info.peer_id;
    address    = session.my_addr;
    left       = BlockSet.bytes_left session.blocks;
    event      = reason;
  } in

  (* send request, get response *)
  Protocol.do_tracker_request session.meta_info request >>= function
    | `Failed -> begin
                   printf "couldn't connect to tracker\n";
                   printf "   will try again in 5 seconds\n";
                   after (Core.Std.sec 5.) >>= fun () ->
                   interact_with_tracker session `Regular ()
                 end
    | `Success message ->

  printf "handling tracker response";

  (* connect to all new peers *)
  List.iter begin fun addr ->
    upon (Protocol.connect_to_peer session.meta_info addr) begin function
      | `Failed             -> ()
      | `Success connection -> add_connection session connection
    end
  end message.peers;

  (* schedule another update request *)
  let delay = Core.Std.sec (float_of_int message.interval) in
  after delay >>= (interact_with_tracker session `Regular)

let create metainfo =
  BlockSet.create metainfo >>= fun blocks ->
  printf "got blocks\n";

  Protocol.listen_for_peers   metainfo >>= function
    | `Failed -> failwith "Failed to open tracker port"
    | `Success (address, connections) ->
  printf "got server\n";

  let (peers_r, peers_w) = Pipe.create () in

  let t = {
    current_peers = Hashtbl.create 7;
    all_peers_r   = peers_r;
    all_peers_w   = peers_w;
    meta_info     = metainfo;
    my_addr       = address;
    blocks        = blocks;
  } in

  don't_wait_for (interact_with_tracker t `Started ());
  don't_wait_for (Pipe.iter_without_pushback connections (add_connection t));
  return t

let get_all_peers t = t.all_peers_r

let get_current_peers t =
  Hashtbl.fold (fun k v a -> v::a) t.current_peers []

let get_metainfo t = t.meta_info

let get_blockset t = t.blocks

