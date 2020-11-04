(** This file tests that the student's worker obeys the basic map reduce
    protocol.  It starts a server that listens for a particular sequence of
    messages, and then starts the student's worker with a job that should cause
    it to generate that sequence of messages. *)

open Async.Std
open Assertions

module NullJob = struct
  type input  = unit
  type key    = unit
  type inter  = unit
  type output = unit

  let name = "null.job"

  let map output =
    return []

  let reduce _ =
    return ()
end

module Req  = Protocol.WorkerRequest  (NullJob)
module Resp = Protocol.WorkerResponse (NullJob)

(** The following two functions are the expected behavior of the worker and
    controller on the input [();();()]. *)

let worker_script result _ r w =
  Reader.read_line r >>= fun msg ->
  msg === `Ok "null.job";

  Req.receive r >>= fun msg ->
  msg === `Ok (Req.MapRequest ());
  Resp.send w (Resp.MapResult []);

  Req.receive r >>= fun msg ->
  msg === `Ok (Req.MapRequest ());
  Resp.send w (Resp.MapResult []);

  Req.receive r >>= fun msg ->
  msg === `Ok (Req.MapRequest ());
  Resp.send w (Resp.MapResult []);

  Ivar.fill result true;
  return ()

let controller_script _ r w =
  Writer.write_line w "null.job";

  Req.send w (Req.MapRequest ());
  Resp.receive r >>= fun msg ->
  msg === `Ok (Resp.MapResult []);

  Req.send w (Req.MapRequest ());
  Resp.receive r >>= fun msg ->
  msg === `Ok (Resp.MapResult []);

  Req.send w (Req.MapRequest ());
  Resp.receive r >>= fun msg ->
  msg === `Ok (Resp.MapResult []);

  return true

module StudentController = RemoteController.Make(NullJob)
module StudentWorker     = Worker.Make(NullJob)

(* see .mli *)
let run_controller_test () =
  let result = Ivar.create () in
  let port   = 31201 in
  Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port port) (worker_script result)
    >>= fun _ ->

  RemoteController.init ["localhost",port];
  StudentController.map_reduce [(); (); ()] >>= fun output ->
  output === [];
  Ivar.read result

TEST "controller_protocol" = Thread_safe.block_on_async_exn run_controller_test

let init port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.on_port port)
    (fun _ r w -> Reader.read_line r >>= fun msg ->
                  msg === `Ok "null.job";
                  StudentWorker.run r w)

let run_worker_test () =
  let port = 31101 in
  init port >>= fun _ ->
  Tcp.with_connection (Tcp.to_host_and_port "localhost" port) controller_script

TEST "worker_protocol" = Thread_safe.block_on_async_exn run_worker_test


