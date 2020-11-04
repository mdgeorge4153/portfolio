open TilePuzzle;;

(** generate the puzzle *******************************************************)

Random.self_init ();;

let scramble = Array.init 50 (fun _ -> Util.choice [N; S; E; W])
let tiles    = TilePuzzle.of_list 3 [0; 1; 2; 3; 4; 5; 6; 7; 8]
let initial  = Array.fold_left (fun state move -> apply state move) tiles scramble

(** run the solver ************************************************************)

module S = Solver.Make(TilePuzzle)  

let solution = S.solve initial

(** run the animation *********************************************************)

module A = Animation.Make(TilePuzzle)

let () = match solution with
  | Some p -> A.run initial p
  | None   -> A.run initial []


