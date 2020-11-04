open PQueue
open Util
open Assertions
open QCheck.Arbitrary

(* ************************************************* *)
let (===) x y = (Assertions.(===) x y); x=y
let is_none x = (Assertions.assert_is_none x); x=None
let not_none x = (Assertions.assert_is_not_none x); x<>None


module H = HeapImpl (* their Heap implementation*)

(* module HeapImpl:PQ = struct

  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  type 'a t = {
    size:  int;
    elems: 'a tree;
    cmp:   'a comparator;
  }

  type dir = Left | Right

  (* returns the path from the root to the last node in a complete binary tree
     with size s *)
  let rec path_to_last s =
    let rec helper a s =
      if s < 1 then failwith "tree is empty"
      else if s = 1 then a
      else let dir = if s land 1 = 0 then Left else Right in
           helper (dir::a) (s lsr 1)
    in helper [] s

  let insert x {size=s; elems=t; cmp=cmp} =
    let min x y = match cmp x y with | Lt -> x | Eq | Gt -> y in
    let max x y = match cmp x y with | Lt -> y | Eq | Gt -> x in
  
    let rec insert x t path = match path, t with
      | [],        Leaf           -> Node (x, Leaf, Leaf)
      | Left::tl,  Node (y, l, r) -> Node (max x y, insert (min x y) l tl, r)
      | Right::tl, Node (y, l, r) -> Node (max x y, l, insert (min x y) r tl)
      | _, _ -> failwith "invariant fail: tree has wrong size or shape"
    in
  
    {size=s+1; elems=insert x t (path_to_last (s+1)); cmp=cmp}
  
  let remove {size=s; elems=t; cmp=cmp} =
    let lt x y = cmp x y = Lt in
    (* removes and returns the last element in the tree *)
    let rec remove t path = match path, t with
      | [],        Node (x, Leaf, Leaf) -> x, Leaf
      | Left::tl,  Node (x, l, r)       -> let v,l' = remove l tl in
                                           v, Node(x, l', r)
      | Right::tl, Node (x, l, r)       -> let v,r' = remove r tl in
                                           v, Node(x, l, r')
      | _ -> failwith "invariant broken: tree is wrong size or shape"
    in
  
    (* given heaps l and r, return a new valid heap containing x, l and r *)
    let rec repair x l r = match l, r with
      | Leaf, Leaf -> Node (x, l, r)
      | Node (y, Leaf, Leaf), Leaf ->
          if lt x y
          then Node (y, Node(x, Leaf, Leaf), Leaf)
          else Node (x, Node(y, Leaf, Leaf), Leaf)
      | Node (yl, ll, lr), Node (yr, rl, rr) ->
          if lt yl x && lt yr x then Node(x, l, r)
          else if lt yl yr then Node(yr, l, repair x rl rr)
          else Node(yl, repair x ll lr, r)
      | _ -> failwith "invariant broken: tree is wrong size or shape"
    in
  
    (* TODO: this is pretty ugly *)
    match t with
      | Leaf -> None
      | Node (x, l, r) -> let last, rest = remove t (path_to_last s) in
        match rest with
          | Leaf -> Some (x, {size=0; elems=Leaf; cmp=cmp})
          | Node (y, l, r) -> Some (y, {size=s-1; elems=repair last l r; cmp=cmp})

  let size h = h.size
  let is_empty h = h.size = 0

  let empty cmp = {size = 0; elems = Leaf; cmp=cmp}

  let comparator h = h.cmp

  let max {elems = t} = match t with
    | Leaf           -> None
    | Node (x, _, _) -> Some x

  (* debugging utils *)

  let rec first n h =
    if n = 0 then h,[] else
    match remove h with
      | Some (x, h') -> let h'', l = first (pred n) h' in h'', x::l
      | None -> h,[]

  let elts h =
    let rec elts t = match t with
      | Leaf -> []
      | Node (x, l, r) -> x :: (elts l) @ (elts r)
    in elts h.elems
end *)

let cmp a b = 
  if a < b then Lt
  else if a > b then Gt
  else Eq 

let cmp2 a b = 
  if a < b then Gt 
  else if a > b then Lt
  else Eq  

(* QCheck Functions *)
let timed n f = Assertions.timeout n f ()

let rand = Random.init 3110

let gen_rand_list l =
  let rand_array = Array.init l (fun (x:int) -> (Random.int 3110)) in
  Array.to_list rand_array 

let qcheck cases test = Assertions.assert_qcheck cases test; true

(* Lowest to highest *)
let get_list_order l cmp = 
  let sorting a b = 
    match cmp a b with
    | Lt -> -1 
    | Gt -> 1
    | Eq -> 0 in 
    List.fast_sort sorting l   

let rec remove_all pq acc = 
  match H.remove pq with 
  | None -> acc 
  | Some (x ,y) -> remove_all y (x::acc)

let rec insert_all pq l = 
  match l with 
  |[] -> pq
  |h::t -> insert_all (H.insert h pq) t

let remove_duplicates lst = 
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] lst

(* ***************Unit Testing********************  *)
(* let one = (H.insert 1 (H.empty cmp))) *)

let test_empty () = 
	(H.is_empty (H.empty cmp)) === true

TEST "empty" = timed 1 test_empty 

let test_max () = (is_none (H.max(H.empty cmp))) && (not_none (H.max((H.insert 1 (H.empty cmp)))))
TEST "max" = timed 1 test_max

let test_size () = 
  ((H.size(H.empty cmp)) === 0) && ((H.size (H.insert 1 (H.empty cmp))) === 1) 
TEST "size" = timed 1 test_size

let test_is_empty () = 
  ((H.is_empty(H.empty cmp)) === true) && ((H.is_empty (H.insert 1 (H.empty cmp))) === false)
TEST "is_empty" = timed 1 test_is_empty

(* ***************insert testing*******************  *)
let insert_empty () = ((H.size (H.insert 1 (H.empty cmp))) === 1)  
TEST "insert_empty" = timed 1 insert_empty


let check_order pq =
	let initial = (gen_rand_list (Random.int 300)) in
	let pq = insert_all pq initial in 
	let result_list = remove_all pq [] in 
	(get_list_order initial cmp) === (result_list)

let check_order_no_dup pq = 
  let initial = remove_duplicates (gen_rand_list (Random.int 3110)) in 
  let pq = insert_all pq initial in 
  let result_list = remove_all pq [] in
  (get_list_order initial cmp) === (result_list)

let check_order1 l cmp = 
  let pq = insert_all (H.empty cmp) l in
  let result_list = remove_all pq [] in 
  (get_list_order l cmp) === (result_list)

let check_order_long pq =
	let initial = (gen_rand_list 5000) in
	let pq = insert_all pq initial in 
	let result_list = remove_all pq [] in 
	(get_list_order initial cmp) === (result_list)

let insert_check_comp () = 
  (* I should randomly generate this *)
  let sample_list = [2;3;5;1;1;2;3;5;7;3] in 
  let first = check_order1 sample_list cmp in 
  let second = check_order1 sample_list cmp2 in 
  first && second

TEST "insert_check_comp" = timed 5 insert_check_comp

let rand_check () = begin
  timed 5 (fun () ->
    qcheck (return (H.empty cmp)) (check_order))      
  end

let rand_check_no_duplicates () = begin
  timed 5 (fun () -> qcheck (return (H.empty cmp)) (check_order_no_dup))
end

TEST "insert_rand_no_dup" = rand_check_no_duplicates ()
TEST "remove_rand_no_dup" = rand_check_no_duplicates ()


TEST "insert_rand" = rand_check ()

(* *****************remove testing****************** *)
let remove_empty () = is_none (H.remove (H.empty cmp))
TEST "remove_empty" = timed 3 remove_empty

TEST "remove_rand" = rand_check ()

(* ***************performance testing*************** *)

(* testing on very large input, this should be still fast w/heap *)
let perf_test () = 
  begin timed 10 (fun () -> 
    qcheck (return (H.empty cmp)) (check_order_long)
  )
  end

TEST "pq_performance" = perf_test ()
