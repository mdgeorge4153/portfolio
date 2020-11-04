(** A puzzle that can be animated *)
module type PUZZLE = sig
  include Solver.PUZZLE

  (** The state of an animation window *)
  type animstate

  (** initialize a new window for drawing on *)
  val init     : state     -> animstate

  (** [render astate state (Some(percent, move))]
      draws the given state of the puzzle on the window defined by [astate],
      with [move] partially applied.  [percent] is a number between [0.] and
      [1.] that determines how much [move] is applied.

      [render astate state None] simply renders state without any move applied.
      It is the same as [render astate state (Some(0.,m))]. *)
  val render   : animstate -> state -> (float*move) option -> unit
end

module Make (Puzzle : PUZZLE) : sig
  open Puzzle

  (** Given an initial state [s] and a list of moves, loop an animation of the
      application of those moves to the initial state. *)
  val run : state -> move list -> unit
end
 
