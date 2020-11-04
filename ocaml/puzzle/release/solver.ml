
(** See solver.mli *)
module type PUZZLE = sig
  type state
  type move

  val apply: state -> move -> state
  val moves: state -> move list
  val is_goal: state -> bool
  val equal: state -> state -> bool
  val goodness: state Util.comparator
end

(** See solver.mli *)
module type S = functor (Puzzle:PUZZLE) -> sig
  val solve : Puzzle.state -> Puzzle.move list option
end

(** A depth-first implementation of the Solver interface *)
module MakeUnguided (Puzzle:PUZZLE) = struct
  open Puzzle

  let solve state =
    (* returns a list of states traversed and optionally a solution *)
    let rec helper path state seen =
      if      is_goal state                  then state::seen, Some []
      else if List.exists (equal state) seen then state::seen, None
      else let rec loop moves seen = match moves with
        | []          -> seen, None
        | move::moves -> match helper (move::path) (apply state move) seen with
           | seen, Some path -> seen, Some (move::path)
           | seen, None      -> loop moves seen
      in loop (moves state) (state::seen)
    in snd (helper [] state [])
end

module Make (Puzzle:PUZZLE) = struct
  let solve state =
    failwith "Do you like green eggs and ham?"
end

