open Util
open Solver
open Assertions
open QCheck.Arbitrary

(* The unsolvable puzzle *)
module U = struct
	type state = One | Two | Three
	type move = Move

	let apply s m = 
		match s with 
		|One -> Two
		|Two -> Three
		|Three -> One
  
  let moves s = [Move]

  let is_goal s = false

  let equal s1 s2 = 
  	match s1,s2 with
  	| One,One |Two,Two |Three,Three -> true
  	| _ -> false

  let goodness s1 s2 = Util.Eq	
end

(* Just an FSM style puzzle. Should add more branches, and moves*)
module Goal = struct
	type state = One | Two | Three | Four | Five
	type move = Move of state

	let apply s m = match m with 
	|Move(x) -> x

	let moves s = 
		match s with 
		|One | Two -> [Move(Three);Move(Four)]
		|Three -> [Move(Three); Move(Five)]
		|Four -> [Move(Five)]
		|Five -> [Move(One); Move(Two)]

	let is_goal s = (s = Five)	

	let equal s1 s2 = 
		match s1,s2 with 
		|One,One | Two,Two | Three,Three | Four,Four | Five,Five -> true
		| _ -> false

	let goodness s1 s2 = 
		let get_num = function
			|One -> 1 
			|Two -> 2 
			|Three -> 3 
			|Four -> 4 
			|Five -> 5 
		in 
		let x = get_num s1 in 
		let y = get_num s2 in 
		if x > y then Util.Gt
		else if x < y then Util.Lt
		else Util.Eq	
end

(* Module that catches if no goodness used *)
module Catch = struct
	type state = Zero |Point_five | One | Three_halves | Two | Three 
	type move = Move of state

	let apply s m = 
		match s with 
		| Zero -> failwith "Should never get here"
		| _ -> 
			begin match m with 
			|Move(x) -> x
			end

	let moves s = 
		match s with
		|One -> [Move(Three_halves);Move(Two)]		
		|Three_halves -> [Move(Three)]
		|Two -> [Move(Point_five)]
		|Three -> [Move(Zero)]
		|Zero -> [Move(Zero)]
		|Point_five -> [Move(Point_five)]


	let is_goal s = (s=Point_five)
	
	let equal s1 s2 =
		match s1, s2 with 
		|One,One | Three_halves,Three_halves | Two,Two | Three,Three -> true
		| _ -> false

	let goodness s1 s2 = 
		let get_num = function
			|Zero -> 0.
			|Point_five -> 0.5
			|One -> 1. 
			|Three_halves -> 1.5
			|Two -> 2.
			|Three -> 3.  
		in 
		let x = get_num s1 in 
		let y = get_num s2 in
		if x > y then Util.Gt
		else if x < y then Util.Lt
		else Util.Eq 			
end

(* Testing with tile puzzle *)
module Tile = struct 
	(** row, col *)
	type pos  = int * int
	type move = N | S | E | W
	type tile = int

	type state = {
	  (* invariant: pieces is an nxn matrix containing the integers [0, n*n) *)
	  pieces: tile array array;

	  (* precomputed values *)
	  blank:   pos; (* computed by find 0 pieces *)
	  badness: int; (* computed by manhattan_distance pieces *)
	}

	let size  st    = Array.length st.pieces

	let equal s1 s2 = s1.pieces = s2.pieces

	let is_goal s   = s.badness = 0

	let goodness s1 s2 =
	  if      s1.badness < s2.badness then Util.Gt
	  else if s1.badness = s2.badness then Util.Eq
	  else (* s1.badness > s2.badness *)   Util.Lt

	(****************************************************************************)
	(* utilities ****************************************************************)
	(****************************************************************************)

	let lookup st (i,j) = st.pieces.(i).(j)
	let int_of_pos s (i,j) = s*i + j
	let pos_of_int s n     = n / s, n mod s

	(** folds f over the array, passing the index as the first argument *)
	let fold_arri f acc arr =
	  snd (Array.fold_left (fun (i,a) n -> i + 1, f i a n) (0,acc) arr)

	(** folds f over the matrix, passing the row and column as the first argument *)
	let fold_mati f =
	  fold_arri (fun i ->
	    fold_arri (fun j ->
	      f (i,j)
	    )
	  )

	(** returns the index of n in a.  Fails if n is not in a *)
	let find n a =
	  match fold_mati (fun p a n' -> if n = n' then Some p else a) None a with
	    | Some p -> p
	    | None   -> failwith "no element found"

	(****************************************************************************)
	(** distance heuristic ******************************************************)
	(****************************************************************************)

	(** the sum over all the tiles of the (manhattan) distance from the current
	    location to the desired location *)
	let manhattan_distance pieces =
	  let size = Array.length pieces in
	  fold_mati (fun (i,j) a n -> let (i',j') = pos_of_int size n in
	                                   let d = abs (i - i') + abs (j - j') in
	                                   a + d
	                 ) 0 pieces

	(****************************************************************************)
	(* creation *****************************************************************)
	(****************************************************************************)

	(* Creates a game board *)
	let create pieces =
	  {
	    pieces  = pieces;
	    blank   = find 0 pieces;
	    badness = manhattan_distance pieces;
	  }

	let of_list n tiles =
	  create (
	    Array.init n (fun i ->
	      Array.init n (fun j ->
	        List.nth tiles (int_of_pos n (i,j))
	      )
	    )
	  )

	let goal n =
	  create (
	    Array.init n (fun i ->
	      Array.init n (fun j ->
	        int_of_pos n (i,j)
	      )
	    )
	  )

	(****************************************************************************)
	(* transitions **************************************************************)
	(****************************************************************************)

	(* [offset pos dir] is the position found by starting at pos and moving in dir *)
	let offset (r,c) = function
	    N -> (r-1, c)
	  | S -> (r+1, c)
	  | E -> (r, c+1)
	  | W -> (r, c-1)

	(* valid_move p m  is whether the move m is valid on puzzle state p. *)
	let valid_move p m = 
	  let r',c' = offset p.blank m in
	  let n     = size p in
	  0 <= r' && r' < n && 0 <= c' && c' < n  

	(* apply p m  is the puzzle that results when the blank space
	   is swapped with the tile in the specified direction. If there
	   is no tile in that direction, returns p. *)
	let apply (p:state) (m:move) : state =
	  if not (valid_move p m) then p
	  else
	    let n         = size p in
	    let oldblank  = p.blank in
	    let newblank  = offset oldblank m in
	    let newpieces = Array.init n (fun i ->
	                      Array.init n (fun j ->
	                        if      (i,j) = oldblank then lookup p newblank
	                        else if (i,j) = newblank then lookup p oldblank
	                        else                          lookup p (i,j)
	                      )
	                    )
	    in create newpieces

	(* This allows us to generate a list of valid moves from a state *)
	let moves (p: state) : move list = 
	  List.filter (valid_move p) [N;E;S;W]
end

(* Creating modules for the different games *)
module Stu = Make(U)

module Stu1 = Make(Goal)

module Stu2 = Make(Catch)

module Stu3 = Make(Tile)


(* ************************************************************ *)
let (===) x y = (Assertions.(===) x y); x=y 
let qcheck cases test = Assertions.assert_qcheck cases test; true

let timed n f = Assertions.timeout n f ()	

(* let rand_state p = 
	let i = Random.init 3110  *)

(* Beginning tests! *)
let start_at_goal_test () = (Stu1.solve (Goal.Five)) === (Some [])
TEST "start_at_goal" = timed 2 (start_at_goal_test)

let unreachable_test () = (Stu.solve (U.One)) === None
TEST "unreachable" = timed 5 (unreachable_test)

let uses_goodness_test () = 
	try 
	match Stu2.solve (Catch.One) with 
	| None -> false
	| Some x -> true
	with Failure _ -> false

TEST "uses_goodness" = (timed 8 uses_goodness_test) === true

let solve_rand () =  
	let helper () =
		let scramble = let open Tile in Array.init 50 (fun _ -> Util.choice [N;S;E;W]) in 
		let tiles = Tile.of_list 3 [0;1;2;3;4;5;6;7;8] in 
		let initial = Array.fold_left (fun state move -> Tile.apply state move) tiles scramble in 
		let solution = Stu3.solve initial in 
		match solution with 
		| None -> false
		| Some _ -> true in 
	let rec run k = 
		if k = 0 then true
		else let solved = helper () in solved && (run (k-1)) in
	run 50 
	
(* Might need ot be changed to run faster *)
TEST "solve_rand" = (timed 10 (solve_rand)) === true

TEST "solve_rand2" = (timed 10 (solve_rand)) === true

TEST "solve_rand3" = (timed 10 (solve_rand)) === true

TEST "solve_rand4" = (timed 10 (solve_rand)) === true

TEST "solve_rand5" = (timed 10 (solve_rand)) === true

(* Need to test path tiebreaking here *)



