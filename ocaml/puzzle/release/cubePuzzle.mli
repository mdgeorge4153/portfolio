(** The Rubic's cube implementation of [Solver.PUZZLE] *)

type state

(** each move rotates the corresponding face of the cube *)
type move = U (** up    *)
          | D (** down  *)
          | L (** left  *)
          | R (** right *)
          | F (** front *)
          | B (** back  *)

include Solver.PUZZLE    with type state := state and type move := move
include Animation.PUZZLE with type state := state and type move := move

(** the solved cube *)
val goal : state

