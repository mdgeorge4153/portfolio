open Util

(** See pQueue.mli *)
module type PQ = sig

  type 'a t
  val empty    : 'a comparator -> 'a t
  val insert   : 'a -> 'a t -> 'a t
  val remove   : 'a t -> ('a * 'a t) option
  val max      : 'a t -> 'a option
  val size     : 'a t -> int
  val is_empty : 'a t -> bool

  val comparator : 'a t -> 'a comparator

end

(******************************************************************************)
(** List-backed priority queue implementation *********************************)
(******************************************************************************)
module ListImpl = struct
  (* invariant: the list is sorted by comparator (max is at head *)
  type 'a t = 'a list * 'a comparator

  let empty cmp = [], cmp

  let insert x (l, cmp) =
    let rec insert l = match l with
      | [] -> [x]
      | y::xs when cmp x y = Lt -> y::(insert xs)
      | _ -> x::l
    in (insert l, cmp)

  let remove (l, cmp) = match l with
    | []    -> None
    | x::xs -> Some (x, (xs,cmp))

  let max (l, _) = match l with
    | []    -> None
    | x::xs -> Some x

  let size     (l, _) = List.length l
  let is_empty (l, _) = l = []

  let comparator (_,cmp) = cmp
end

(******************************************************************************)
(** Heap-backed priority queue implementation *********************************)
(******************************************************************************)
module HeapImpl = struct

  type 'a t (* NOTE: you should define this type *)
  type dir = Left | Right

  (** returns the path from the root to the last node in a complete binary tree
      with size s *)
  let rec path_to_last s =
    let rec helper a s =
      if s < 1 then failwith "tree is empty"
      else if s = 1 then a
      else let dir = if s land 1 = 0 then Left else Right in
           helper (dir::a) (s lsr 1)
    in helper [] s

  let empty    cmp  = failwith "What are you doing?"
  let insert   x pq = failwith "Spinning counterclockwise"
  let remove   pq   = failwith "Each turn robs the planet of angular momentum"
  let max      pq   = failwith "slowing its spin the tiniest bit"
  let size     pq   = failwith "lengthening the night, pushing back the dawn"
  let is_empty pq   = failwith "giving me a little more time here"

  let comparator pq = failwith "with you"

end

(******************************************************************************)
(** Heapsort ******************************************************************)
(******************************************************************************)
module Heapsort (PQ : PQ) = struct

  let rec to_list h = match PQ.remove h with
    | None        -> []
    | Some (x,h') -> x::to_list h'

  let rec of_list cmp l = List.fold_right PQ.insert l (PQ.empty cmp)

  let sort compare l =
    let cmp x y =
      let n = compare x y in
      if n < 0 then Lt
      else if n = 0 then Eq
      else Gt
    in

    to_list (of_list cmp l)

end

