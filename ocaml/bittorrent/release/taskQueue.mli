open Async.Std

module type TaskSpec = sig
  type input
  type output

  type worker

  (** Returns true if the two workers are the same *)
  val same_worker : worker -> worker -> bool

  (** priority i1 i2 returns `Less if i1 is of lower priority than i2 *)
  val priority    : input  -> input  -> [`Less | `Equal | `Greater]

  (** Returns the maximum number of concurrent jobs the worker can do *)
  val max_concurrent_jobs : worker -> int

  (** Returns true if the worker can do the input *)
  val can_do_job          : worker -> input -> bool

  (** Becomes determined when the worker has changed and returns this new worker *)
  val worker_changed      : worker -> worker Deferred.t

  (** Tells the worker to work on the job specified by the input to return a resuilt *)
  val do_task  : worker -> input -> output option Deferred.t

  (** Cancels the task assigned to the worker. Does nothing if not assigned *)
  val cancel_task : worker -> input -> unit
end

module Make (Spec : TaskSpec) : sig
  type t

  (** Create the TaskQueue *)
  val create : unit -> t

  (** Add a worker to the TaskQueue. Replace worker if already in the queue *)
  val add_worker    : t -> Spec.worker -> unit

  (** Adds the job to the task queue and returns the output once it becomes determined *)
  val do_job : t -> Spec.input -> Spec.output Deferred.t
end

