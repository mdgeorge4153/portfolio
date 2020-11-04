(* Core.Std overloads Bytes with a broken module.  We save the original and then
   redefine it after opening Core.Std. *)
module B = Bytes
open Core.Std
module Bytes = B

open Async.Std
open BExpr
open Metainfo

(******************************************************************************)
(** type definitions, see .mli  ***********************************************)
(******************************************************************************)

type inet_address = Unix.Inet_addr.t * int

module Tracker = struct
  type request = {
    info_hash  : SHA1.t;
    peer_id    : Metainfo.peer_id;
    address    : inet_address;
    left       : int64;
    event      : [`Regular | `Started | `Completed | `Stopped];
  }

  type response = {
    interval : int;
    peers    : inet_address list;
  }
end

module Peer = struct

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
    | Piece    of BlockSet.block_id * Bytes.t
end

type peer_connection    = Metainfo.peer_id * Peer.message Pipe.Reader.t * Peer.message Pipe.Writer.t
type tracker_connection = Tracker.response Pipe.Reader.t * Tracker.request Pipe.Writer.t

(******************************************************************************)
(** Encoding tracker requests *************************************************)
(******************************************************************************)

open Tracker
open Peer


let query_of_request req : (string * string) list =
  (* this is a bit of a hack: we report all of our I/O in
   * the uploaded and downloaded fields *)
  let downloaded = Int63.to_int64 (Io_stats.total Reader.io_stats) in
  let uploaded   = Int63.to_int64 (Io_stats.total Writer.io_stats) in
  let dict = [("downloaded", Int64.to_string downloaded);
              ("info_hash",  SHA1.to_bytes req.info_hash);
              ("left",       Int64.to_string req.left);
              ("peer_id",    SHA1.to_bytes req.peer_id);
              ("port",       string_of_int (snd req.address));
              ("uploaded",   Int64.to_string uploaded)] in
  let event_helper s = ("event", s) in
  match req.event with
  | `Regular   -> dict
  | `Started   -> (event_helper "started") :: dict
  | `Completed -> (event_helper "completed") :: dict
  | `Stopped   -> (event_helper "stopped") :: dict

(* val encode_request : Metainfo.t -> request -> string *)
let encode_request metainfo req =
  printf "encoding tracker request\n";
  let uri = Uri.add_query_params' metainfo.announce (query_of_request req) in
  "GET " ^ (Uri.path_and_query uri) ^ " HTTP/1.1\n\n"

(******************************************************************************)
(** decoding tracker responses ************************************************)
(******************************************************************************)

(* val parse_tracker_resp : Bexpr.t -> response Read_result.t*)
let extract_tracker_response expr =

  (* this function has a helper to parse each sub-part of a tracker response:
   *  - parse_peer
   *  - parse_peer_list
   *  - parse_peer_list_compact
   *  - parse_request
   * these functions all return Reader.Read_result.t's.  The monad operations
   * from Reader.Read_result are used to manage failure.  Note that because we
   * open Reader.Read_result here, (>>=) and return operate on Read_result.t's,
   * not Deferred.t
   *)
  let open Reader.Read_result in

  (* parse contents of a BDict representing a peer, return address and port *)
  let parse_peer peer =
    begin match List.Assoc.find peer "ip" with
      | Some (BString addr) -> return (Unix.Inet_addr.of_string addr)
      | _                   -> printf "no address\n"; `Eof
    end >>= fun address ->

    begin match List.Assoc.find peer "port" with
      | Some (BInt port) -> return (Int64.to_int_exn port)
      | _                -> printf "no port\n"; `Eof
    end >>= fun port ->

    return (address, port)
  in

  (* parse contents of a BList representing a peer list (long form) *)
  let rec parse_peer_list (peers : BExpr.t list) = match peers with
    | (BDict dict)::tl -> parse_peer dict    >>= fun peer ->
                          parse_peer_list tl >>= fun tl   ->
                          return (peer::tl)
    | []               -> return []
    | _ -> printf "no/malformed peer list\n"; `Eof
  in

  let address_of_chars (chars : char list) =
    let open Int32 in
    let i_of_c c = Int32.of_int_exn (int_of_char c) in
    let bytes    = List.map  ~f:i_of_c chars in
    let ints   : Int32.t list = List.mapi ~f:(fun i b -> shift_left b Pervasives.(24 - 8*i)) bytes in
    let result   = List.fold_left ints ~f:bit_or ~init:Int32.zero in
    Unix.Inet_addr.inet4_addr_of_int32 result
  in

  (* parse contents of a BString representing a peer list (compact form) *)
  let parse_peer_list_compact (peers : string) =
    let rec helper chars = match chars with
      | []                         -> return []
      | c1::c2::c3::c4::c5::c6::tl -> helper tl >>= fun tl ->
                                      let addr = address_of_chars [c1;c2;c3;c4] in
                                      let port = ((int_of_char c5) lsl 8) lor
                                                 ((int_of_char c6) lsl 0) in
                                      return ((addr, port)::tl)
      | _                          -> printf "compact peer list not multiple of 6\n";
                                      `Eof
    in helper (String.to_list peers)
  in

  (* parse contents of a BDict representing a tracker request *)
  let parse_request dict =
    begin match List.Assoc.find dict "interval" with
      | Some (BInt n) -> return (Int64.to_int_exn n)
      | _             -> printf "no interval\n"; `Eof
    end >>= fun interval ->

    begin match List.Assoc.find dict "peers" with
      | Some (BList   peers) -> parse_peer_list         peers
      | Some (BString peers) -> parse_peer_list_compact peers
      | _                    -> printf "no peers\n"; `Eof
    end >>= fun peers ->

    return {
      interval = interval;
      peers    = peers;
    }
  in

  match expr with
    | BDict dict -> parse_request dict
    | _          -> printf "not a dictionary\n"; `Eof

let parse_tracker_response reader =
  printf "parsing tracker response\n";
  let rec consume_http_headers () =
    Reader.read_line reader >>= function
      | `Ok ""  -> return ()
      | `Ok hdr -> consume_http_headers ()
      | `Eof    -> return ()
  in
  consume_http_headers () >>= fun () ->
  BExpr.decode reader >>= function
    | `Failure       -> printf "no bexpr\n"; return `Eof
    | `Success bexpr -> return (extract_tracker_response bexpr)


(******************************************************************************)
(** encoding peer messages ****************************************************)
(******************************************************************************)

(** encode a peer message as a string according to the peer msg protocol. *)
let encode_peer_msg msg : string =
  printf "encoding peer message\n";

  (** encode an int as a 4-byte big-endian string *)
  let encode_int n : string =
    let bytes = Bytes.create 4 in
    Bytes.set bytes 0 (char_of_int (0xff land n lsr 24));
    Bytes.set bytes 1 (char_of_int (0xff land n lsr 16));
    Bytes.set bytes 2 (char_of_int (0xff land n lsr  8));
    Bytes.set bytes 3 (char_of_int (0xff land n lsr  0));
    Bytes.to_string bytes
  in

  (** encode a bitfield compactly as a sequence of bytes. *)
  let encode_bitfield bits : string =
    let open Bitfield in
    let length = length bits / 8 + if length bits mod 8 > 0 then 1 else 0 in
    let bytes  = Bytes.make length '\x00' in

    for i = 0 to Bitfield.length bits do
      if is_set bits i then
        let index     = i / 8 in
        let offset    = i mod 8 in
        let old_byte  = int_of_char (Bytes.get bytes index) in
        let new_byte  = char_of_int (old_byte lor (0x1 lsl offset)) in
        Bytes.set bytes index new_byte
    done;
    Bytes.to_string bytes
  in

  (** encode a block id *)
  let encode_block_id b_id =
    let open BlockSet in
    (encode_int b_id.piece) ^
    (encode_int b_id.offset) ^
    (encode_int b_id.length)
  in

  (** encode the payload of a Piece message *)
  let encode_piece block_id bytes =
    let open BlockSet in
    (encode_int block_id.piece)  ^
    (encode_int block_id.offset) ^
    bytes
  in

  let str = match msg with
    (** strings are length prefixed.  Variants with no data will have length
        '\001', while variants with data will have larger lengths. *)
    | Keepalive       -> ""
    | Choke           -> "\x00"
    | Unchoke         -> "\x01"
    | Interested      -> "\x02"
    | Uninterested    -> "\x03"
    | Have     p_id   -> "\x04" ^ (encode_int      p_id)
    | Bitfield bits   -> "\x05" ^ (encode_bitfield bits)
    | Request  b_id   -> "\x06" ^ (encode_block_id b_id)
    | Piece (b_id, b) -> "\x07" ^ (encode_piece    b_id b)
    | Cancel   b_id   -> "\x08" ^ (encode_block_id b_id)
  in

  (encode_int (String.length str)) ^ str

(******************************************************************************)
(** decoding peer messages ****************************************************)
(******************************************************************************)

module ReadResultDeferred : sig
  type 'a t = 'a Reader.Read_result.t Deferred.t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|)  : 'a t -> ('a -> 'b)   -> 'b t
  val return : 'a -> 'a t
  val fail   : 'a t
end = struct
  type 'a t = 'a Reader.Read_result.t Deferred.t

  let (>>=) m f =
    m >>= function
      | `Eof  -> return `Eof
      | `Ok v -> f v

  let fail = return `Eof

  let return x = return (`Ok x)

  let (>>|) m f =
    m >>= fun x -> return (f x)

end

(* val parse_peer_msg : Metainfo.t -> Reader.t -> Peer.message Read_result.t Deferred.t *)
let parse_peer_msg metainfo reader =
  let open ReadResultDeferred in

  (** read a 4-byte big-endian integer *)
  let read_int () =
    Reader.read_char reader >>| int_of_char >>= fun c1 ->
    Reader.read_char reader >>| int_of_char >>= fun c2 ->
    Reader.read_char reader >>| int_of_char >>= fun c3 ->
    Reader.read_char reader >>| int_of_char >>= fun c4 ->
    return ((c1 lsl 24) lor (c2 lsl 16) lor (c3 lsl 8) lor (c4 lsl 0))
  in

  (** read a bitfield *)
  let read_bitfield length =
    let num_pieces = Array.length metainfo.piece_hashes in
    let num_bytes  = num_pieces / 8 in
    let buf        = Bytes.create num_pieces in

    Deferred.(
      Reader.really_read ~len:num_bytes reader buf >>= function
        | `Eof _ -> return (`Eof)
        | `Ok    -> return (`Ok ())
    ) >>= fun () ->

    let result     = Bitfield.create_empty num_pieces in

    for i = 0 to num_bytes do
      let byte = int_of_char (Bytes.get buf i) in
      for bit = 0 to 8 do
        if (byte lsr bit) land 0x1 = 0x1 then
          Bitfield.imp_set result (i*8 + bit)
      done
    done;

    let leftover = num_pieces mod 8 in
    if leftover = 0 then
      return (Bitfield.copy result)
    else
      Reader.read_char reader >>= fun c ->
      let byte = int_of_char c in
      for bit = 0 to leftover do
        if (byte lsr bit) land 0x1 = 0x1 then
          Bitfield.imp_set result (num_bytes * 8 + bit)
      done;
      return (Bitfield.copy result)
  in

  let read_block_id () =
    read_int () >>= fun piece ->
    read_int () >>= fun offset ->
    read_int () >>= fun length ->
    return BlockSet.{piece;offset;length;}
  in

  printf "decoding peer message\n";
  read_int () >>= fun length ->
  if length = 0 then return Keepalive else

  let check_len n result = if n = length then return result else fail in

  Reader.read_char reader >>= function
    | '\x00'  -> check_len 1 Choke
    | '\x01'  -> check_len 1 Unchoke
    | '\x02'  -> check_len 1 Interested
    | '\x03'  -> check_len 1 Uninterested
    | '\x04'  -> begin
                   read_int () >>= fun p_id ->
                   check_len 5 (Have p_id)
                 end
    | '\x05'  -> begin
                   read_bitfield (length - 1) >>= fun bits ->
                   return (Bitfield bits)
                 end
    | '\x06'  -> read_block_id () >>= fun block_id ->
                 return (Request block_id)
    | '\x07'  -> read_bitfield (length - 1) >>= fun bits ->
                 return (Bitfield bits)
    | '\x08'  -> read_block_id () >>= fun block_id ->
                 return (Cancel block_id)
    | _ -> fail

(******************************************************************************)
(** Connecting to peers *******************************************************)
(******************************************************************************)

(*
 * bittorrent handshake:
 *   - 1  byte  '\019'
 *   - 19 bytes "BitTorrent protocol"
 *   - 8  bytes (reserved, currently all '\000'
 *   - 20 bytes hash of metainfo
 *   - 20 bytes peer id
 * total: 68 bytes
 *)

(* sends the handshake to peer given by writer, returns pipe *)
let send_handshake metainfo writer : [`Failed | `Success of Peer.message Pipe.Writer.t] Deferred.t =
  (* send the handshake to the remote peer *)
  let handshake = "\019BitTorrent protocol\000\000\000\000\000\000\000\000" ^
                  (SHA1.to_bytes metainfo.hash) ^
                  (SHA1.to_bytes metainfo.peer_id) in
  Writer.write writer handshake;

  (* set up the output pipe: *)
  let (msg_out_reader, msg_out) = Pipe.create () in
  don't_wait_for
    (Pipe.transfer msg_out_reader (Writer.pipe writer) encode_peer_msg);

  (* return the resulting end of the pipe *)
  printf "send_handshake successful\n";
  return (`Success msg_out)

(* receive handshake from remote peer, returns remote peer id and message pipe *)
let recv_handshake metainfo reader : [`Failed | `Success of Metainfo.peer_id * Peer.message Pipe.Reader.t] Deferred.t =

  printf "receiving handshake\n";

  (* receive handshake from remote peer *)
  Reader.read_char reader >>= fun c ->
  if c <> `Ok '\019' then begin
    printf "didn't get 19\n";
    return `Failed
  end else

  let buf = Bytes.create 20 in
  Reader.really_read reader ~len:19 buf >>= function
    | `Eof _ -> printf "connection closed\n"; return `Failed
    | `Ok    ->
  if Bytes.sub buf 0 19 <> "BitTorrent protocol" then begin
    printf "didn't get BT prot string\n";
    return `Failed
  end else

  Reader.really_read reader ~len:8 buf >>= function
    | `Eof _ -> printf "connection closed\n"; return `Failed
    | `Ok    ->

  Reader.really_read reader ~len:20 buf >>= function
    | `Eof _ -> printf "connection closed\n"; return `Failed
    | `Ok    ->
  let expected = SHA1.to_bytes metainfo.hash in
  let received = Bytes.sub buf 0 20 in
  if expected <> received then begin
    printf "info hashes didn't match (%s <> %s)" expected received;
    return `Failed
  end else

  Reader.really_read reader ~len:20 buf >>= function
    | `Eof _ -> printf "connection closed\n"; return `Failed
    | `Ok    ->
  let remote_peer_id = SHA1.of_bytes (Bytes.sub buf 0 20) in

  (* set up the input message pipe, which takes bytes -> bexprs -> messages *)
  let msg_in = Reader.read_all reader (parse_peer_msg metainfo) in
  return (`Success (remote_peer_id, msg_in))

(* perform the peer handshake. *)
let do_peer_handshake metainfo (reader : Reader.t) (writer : Writer.t) : [`Failed | `Success of peer_connection] Deferred.t =
  let deferred_out = send_handshake metainfo writer in
  let deferred_in  = recv_handshake metainfo reader in

  deferred_out >>= fun output ->
  deferred_in  >>= fun input  ->
  match input, output with
    | `Success (remote_peer_id, msgs_in), `Success (msgs_out) -> return (`Success (remote_peer_id, msgs_in, msgs_out))
    | _ -> return `Failed

(* open TCP to peer, create_peer (in peer.mli) *)
let connect_to_peer metainfo address =
  printf "connect to peer\n";
  try_with begin fun () ->
    (* establish connection *)
    Tcp.connect (Tcp.to_inet_address (`Inet address))
  end >>= function
    | Ok (_, reader, writer) -> do_peer_handshake metainfo reader writer
    | _ -> return `Failed

(******************************************************************************)
(** Connecting to trackers ****************************************************)
(******************************************************************************)

(** Reader.t -> Writer.t -> (response Pipe.Reader.t * request Pipe.Writer.t) *)
let do_tracker_handshake reader writer metainfo =
  let resp_in = Reader.read_all reader parse_tracker_response in
  let (req_out_reader, req_out) = Pipe.create () in

  don't_wait_for
    (Pipe.transfer req_out_reader (Writer.pipe writer) (encode_request metainfo));

  return (`Success (resp_in, req_out))

let do_tracker_request metainfo req =
  match Uri.host metainfo.announce with
    | None      -> return `Failed
    | Some host ->
  let port = match Uri.port metainfo.announce with
    | Some p -> p
    | None   -> 6969 (* Default BitTorrent tracker port. *) in

  printf "connecting to tracker\n";
  try_with begin fun () ->
    Tcp.connect (Tcp.to_host_and_port host port)
  end >>= function
    | Error _              -> return `Failed
    | Ok (_,reader,writer) ->

  Writer.write writer (encode_request metainfo req);
  parse_tracker_response reader >>= function
    | `Eof     -> printf "failed to parse a tracker response\n";
                  return `Failed
    | `Ok resp -> printf "returning tracker response\n";
                  return (`Success resp)


(******************************************************************************)
(** responding to peer connections ********************************************)
(******************************************************************************)

(* set up server socket (maybe search for port that work), then REPEATEDLY listen to connections -> handshake -> create peer object -> throw into pipe  *)
(* open a server socket (listen for a new connection). Once have connection, create pipes then stick into peer_conections *)
let listen_for_peers metainfo =
  let (connection_reader, connection_writer) = Pipe.create () in
  let handle_connection addr reader writer =
    do_peer_handshake metainfo reader writer >>= function
      | `Failed -> return ()
      | `Success connection ->
    Pipe.write_without_pushback connection_writer connection;
    never ()
  in
  let rec connect curr highest =
    if curr > highest then
      return `Failed
    else
      try_with begin fun () ->
        Tcp.Server.create ~on_handler_error: `Ignore
                          (Tcp.on_port curr)
                          handle_connection
      end >>= function
        | Ok server -> return (`Success server)
        | _         -> connect (curr + 1) highest
  in
  connect 6881 6889 >>= function
    | `Failed         -> return `Failed
    | `Success server ->
  let socket = Tcp.Server.listening_socket server in
  let addr   = Socket.getsockname socket in
  let host   = Socket.Address.Inet.addr addr in
  let port   = Socket.Address.Inet.port addr in
  let result = ((host,port), connection_reader) in
  return (`Success result)

