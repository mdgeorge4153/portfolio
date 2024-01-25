open Async.Std

let addresses = ref []

let available_workers = ref 0

let reset_available_workers () =
  available_workers := List.length !addresses

let init addrs =
  addresses := addrs;

module Make (Job : MapReduce.Job) = struct
  module Request  = Protocol.WorkerRequest (Job)
  module Response = Protocol.WorkerResponse (Job)
  module Combiner = Combiner.Make(Job)

  type worker = {
    output : Writer.t;
    input  : Reader.t;
  }

  (** initializes the given mapper and places it into the mappers queue *)
  let init_worker workers (host, port) =
    try_with (fun () -> Tcp.connect (Tcp.to_host_and_port host port))
    >>| function
        | Core.Std.Error _ ->
          print_endline ("Could not connect to "
            ^ host ^ ":" ^ string_of_int port);
          decr available_workers;
          if !available_workers <= 0 then failwith "All workers failed"
        | Core.Std.Ok (_, r, w) ->
          Writer.write_line w Job.name;
          AQueue.push workers {output=w; input=r}

  let do_work workers msg pred =
    let result = Ivar.create () in

    let rec retry () : unit =
      if Ivar.is_empty result then don't_wait_for (
          AQueue.pop workers
          >>= fun worker ->
              Request.send worker.output msg;
              let resd = Response.receive worker.input in
              don't_wait_for(with_timeout (Core.Span.of_sec 30.) resd
                              >>| (function | `Timeout -> retry () | _ -> ()));
              resd
          >>| pred
          >>| function 
              | Some r ->
                  Ivar.fill_if_empty result r;
                  AQueue.push workers worker
              | None ->
                  (* worker gets dropped *)
                  decr available_workers;
                  retry ()
      )
    in
    retry ();
    Ivar.read result

  let do_map workers input =
    do_work workers (Request.MapRequest input) (function
      | `Ok (Response.MapResult rs) -> Some rs
      | `Ok (Response.JobFailed ex) -> failwith ("Map failed: " ^ ex)
      | _ -> None
    )

  let do_reduce workers (key, values) =
    do_work workers (Request.ReduceRequest (key, values)) (function
      | `Ok (Response.ReduceResult output) -> Some (key, output)
      | `Ok (Response.JobFailed ex) -> failwith ("Reduce failed: " ^ ex)
      | _ -> None
    )

  let map_reduce inputs =
    reset_available_workers ();
    let workers = AQueue.create () in
    don't_wait_for (Deferred.List.iter ~how:`Parallel !addresses ~f:(init_worker workers));
    Deferred.List.map ~how:`Parallel ~f:(do_map workers) inputs
    >>| List.flatten
    >>| Combiner.combine
    >>= Deferred.List.map ~how:`Parallel ~f:(do_reduce workers)
    >>= fun results ->
        print_endline ("Done " ^ Job.name);
        return results
end

