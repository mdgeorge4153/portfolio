open Async.Std
open Async_unix
open Simulations

(******************************************************************************)
(** {2 NBody job}                                                             *)
(******************************************************************************)

module Job = struct
  type input
  type key
  type inter
  type output

  let name = "nbody.job"

  let map : input -> (key * inter) list Deferred.t = function
    | _ -> failwith "Standing in the rain, with his head hung low / Couldn't get a ticket, it was a sold out show"

  let reduce : key * inter list -> output Deferred.t = function
    | _ -> failwith "In a town without a name, in a heavy downpour / Thought he passed his own shadow by the backstage door"
end

let () = MapReduce.register_job (module Job)

(******************************************************************************)
(** {2 NBody app}                                                             *)
(******************************************************************************)

module App  = struct
  let name = "nbody"

  (****************************************************************************)
  (** The nbody application                                                   *)
  (****************************************************************************)

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (** a configuration is just a set of bodies *)
    type config = body list

    (** runs n time steps, and returns a list of configs, with the ith config
        representing the ith time step *)
    let run (n : int) (bodies : body list) : config list Deferred.t =
      failwith "So he started hackin' / Ain't never gonna stop / Gotta keep on hackin' / Someday he's gonna make it to the top"

    let main args = match args with
      | [input; iters; output] -> Simulations.read_data input
                                    >>= run (int_of_string iters)
                                    >>= Simulations.write_transcript output
      | _ -> failwith "Usage: nbody <input file> <iters> <output file>"
  end
end

let () = MapReduce.register_app (module App)

