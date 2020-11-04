open Async.Std

(**
 * The purpose of this module is to get all of the blocks of the file as fast
 * as possible.  To do so, you want to keep the peers who are not choking you
 * as busy as possible at all times.  In BitTorrent, "as busy as possible"
 * means that you have at most 3 outstanding requests to any remote peer, and
 * those requests should all be for blocks of pieces that the remote peer has.
 *
 * A good way to solve this problem is to abstract away from thinking about
 * peers and blocks, and instead think more generally about workers and jobs.
 * At any point in time, a worker can do some number of concurrent jobs, and
 * there are some jobs the worker can do, and other jobs that they can't.  The
 * number of jobs a worker can do and the set of jobs that they can do can
 * change over time; we can use the functional observer pattern (as described
 * in recitation 18) to stay abreast of any changes to the workers.  These ideas
 * are captured by the following module type:
 *)

module type TaskSpec = sig
  type job
  type output
  type worker

  (** Returns the maximum number of concurrent jobs the worker can do *)
  val max_concurrent_jobs : worker -> int

  (** Returns true if the worker can do the job *)
  val can_do_job          : worker -> job -> bool

  (**
   * Becomes determined if the set of jobs or the number of jobs that a given
   * worker can do changes.
   *)
  val worker_updated      : worker -> worker Deferred.t

  (**
   * Tells the worker to work on a job. Returns None if the worker fails to do
   * the job for any reason.
   *)
  val do_task  : worker -> job -> output option Deferred.t

  (**
   * Tells the worker to stop working on a job.  Does nothing if the worker
   * isn't actually working on the job
   *)
  val cancel_task : worker -> job -> unit
end

(**
 * Given a task specification, we can write a generic TaskManager module that
 * matches workers and tasks:
 *)
module TaskManager (TS : TaskSpec) = struct
  (**
   * At any point in time, a TaskManager will have some collection of available
   * workers, and some collection of required jobs, and some of the workers will
   * be working on some of the jobs.  You can define a data structure to keep
   * track of this state:
   *)
  type t = unit (* TODO *)

  let create () : t = failwith "TODO"

  (**
   * The TaskManager's goal is to keep the
   * workers as busy as possible, so the following invariant should always be
   * satisfied:
   *
   *   - if a worker can do n jobs concurrently, and if there are at least n
   *     jobs that the worker can do, then the worker should be doing n jobs.
   *
   * We don't want to waste our time working on jobs that are already done:
   *
   *   - no worker should be working on a job if the job is complete.  If a job
   *     is finished by one worker while another worker is still working on it,
   *     then the second worker should be stopped (by calling cancel_task) and,
   *     if possible, given a new job to work on.
   *
   * We also don't want to waste time by having a worker start a task and then
   * stop before the task is done.
   *
   * It is possible for multiple workers to be working separately on the same
   * job, but if a worker is looking for more work, they should prefer to start
   * jobs that nobody else is working on.
   *)

  (**
   * These invariants need to be maintained as the set of workers and jobs
   * change.  These sets could change because other parts of the system give us
   * new work or new workers:
   *)
  let add_worker manager worker : unit                   = failwith "TODO"
  let do_job     manager job    : Spec.output Deferred.t = failwith "TODO"

  (**
   * The state could also change in response to changes from the workers:
   *   - A worker could finish a job
   *   - A worker could be updated to handle a different number or set of jobs
   *
   * The task manager should observe these changes and update its internal state
   * appropriately, calling do_work and cancel on the workers as necessary.
   *)
end

(**
 * Once you have implemented a task manager, you can use it to implement the
 * download manager.
 *
 * You can create a task spec that defines workers using Peer.ts, and
 * implements jobs using block_ids.  You can call Session.get_all_peers to get
 * the peers as they connect, and can call add_worker on your manager when you
 * get one.  You can call BlockSet.need to construct the initial list of jobs,
 * and add them all to the task manager.  The task manager will then take care
 * of keeping your peers busy downloading the file.
 *)

let download : Session.t -> unit Deferred.t = failwith "TODO"

(**
 * There is one last detail that requires attention
 * here.  After all of the blocks in a piece are
 * downloaded, the piece is checked against a hash.
 * If the piece has been corrupted, the hash check
 * will fail, and the piece will have to be downloaded
 * again.
 *
 * The BlockSet module is responsible for checking the hash, but the download
 * manager needs to reassign those blocks to workers.  You can find out when a
 * block has failed to verify by reading from the `BlockSet.failed_to_verify`
 * queue.
 *)

