(** A generic puzzle solver *)

(** A PUZZLE describes the state space of a puzzle. *)
module type PUZZLE = sig
	type state
	type move

  (** [apply s m] the state created by executing the move [m] in state [s] *)
	val apply: state -> move -> state

	(** Gives the list of all possible moves from a given state *)
	val moves: state -> move list

  (** returns true if the given state is a goal state *)
	val is_goal: state -> bool

  (** returns true if two states are equal *)
	val equal: state -> state -> bool

  (** Compares two states based on how close to the goal they are.  [s1]
      is better than [s2] if [goodness s1 s2 = Gt]. *)
  val goodness: state Util.comparator
end

(** Signature of the functors [Solver.Make] and [Solver.MakeUnguided] *)
module type S = functor (Puzzle:PUZZLE) -> sig

  (** Given an initial state [s], output a sequence [[m1; m2; ...]] that when
      applied to [s] in order, produces a goal state.

      Returns [None] if there is no path to a goal. *)
  val solve : Puzzle.state -> Puzzle.move list option
end

(** An inefficient puzzle solver that uses depth-first search *)
module MakeUnguided : S

(** A best-first puzzle solver. *)
module Make : S

