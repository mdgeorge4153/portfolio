open Async.Std

module Make (Job : MapReduce.Job) = struct
  module Request  = Protocol.WorkerRequest(Job)
  module Response = Protocol.WorkerResponse(Job)

  (* TODO: this prevents asynchrony - while a single map job is
      waiting, we could be processing more, but we don't move on to the next one
      until one is complete.  This keeps the responses in the same order as the
      requeusts, which simplifies life somewhat. *)
  let rec run r w =
    let module RQ = Request in
    let module RS = Response in
    let do_work work success =
      try_with work
      >>| (function
          | Core.Std.Error e -> RS.JobFailed (Core.Exn.to_string e)
          | Core.Std.Ok result -> success result)
      >>= fun msg -> RS.send w msg; run r w in

    RQ.receive r
    >>= function
        | `Eof -> print_endline "Client closed connection"; Reader.close r
        | `Ok result -> match result with
          | RQ.MapRequest input ->
            do_work (fun () -> Job.map input) (fun r -> RS.MapResult r)
          | RQ.ReduceRequest (k, vs) ->
            do_work (fun () -> Job.reduce (k, vs)) (fun r -> RS.ReduceResult r)
end

let init port =
  never ()

