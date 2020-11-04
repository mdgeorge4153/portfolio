(** The sliding tile puzzle implementation of [Solver.PUZZLE] *)

(** a sliding tile state *)
type state

(** each move slides the blank space in the corresponding direction *)
type move = N (** North *)
          | S (** South *)
          | E (** East  *)
          | W (** West  *)

type tile = int

include Solver.PUZZLE    with type state := state and type move := move
include Animation.PUZZLE with type state := state and type move := move

(** Given a size [n] and a list [l] of size (n*n), creates a sliding tile puzzle.
    The tiles of [l] are placed from left to right, then from top to bottom. [0]
    represents the blank tile.

    Example: [of_list 2 [0; 3; 1; 2]] creates the board
    {v
    +------+
    |    3 |
    | 1  2 |
    +------+
    v} *)
val of_list : int -> tile list -> state

