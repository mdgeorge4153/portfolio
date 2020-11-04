open CubePuzzle

(** Helper functions **********************************************************)

(* replaces each move in the list with its inverse.  Note that this is not the
   inverse move list. *)
let rec invert moves = match moves with
  | []                              -> []
  | x::y::z::tl when x = y && y = z -> x::(invert tl)
  | x::y::tl when x = y             -> x::y::(invert tl)
  | x::tl                           -> x::x::x::(invert tl)

(* randomly replaces some of the moves with equivalent ones to make a more
   interesting animation. *)
let rec flavor moves =
  let flip = Random.bool () in match moves with
    | []              -> []
    | U::tl when flip -> D::(flavor tl)
    | R::tl when flip -> L::(flavor tl)
    | F::tl when flip -> B::(flavor tl)
    | hd::tl -> hd::(flavor tl)

let flavor moves = moves

(** generate the puzzle *******************************************************)

let () = Random.self_init ()

let sol      = Array.to_list (Array.init 8 (fun _ -> Util.choice [U; R; F]))
let scramble = invert sol
let initial  = List.fold_left apply goal scramble

(** run the solver ************************************************************)

module S = Solver.Make(CubePuzzle)

let sol = S.solve initial

(** run the animation *********************************************************)

module A = Animation.Make(CubePuzzle)

let () = match sol with
  | None   -> A.run initial []
  | Some p -> A.run initial (flavor p)

