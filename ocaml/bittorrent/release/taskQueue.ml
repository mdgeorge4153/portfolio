open Async.Std

module type TaskSpec = sig
  type input
  type output

  type worker

  val same_worker : worker -> worker -> bool

  val priority    : input  -> input  -> [`Less | `Equal | `Greater]

  val max_concurrent_jobs : worker -> int

  val can_do_job          : worker -> input -> bool

  val worker_changed      : worker -> worker Deferred.t

  val do_task  : worker -> input -> output option Deferred.t

  val cancel_task : worker -> input -> unit
end

module Make (Spec : TaskSpec) = struct
  (* Replace this *)
  type t = unit

  let create : unit -> t = failwith "TODO"

  let add_worker    : t -> Spec.worker -> unit = failwith "TODO"

  let do_job : t -> Spec.input -> Spec.output Deferred.t = failwith "TODO"
end