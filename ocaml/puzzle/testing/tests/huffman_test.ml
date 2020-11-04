open QCheck
open Assertions
open Util
open Huffman
(* ***************************************************************** *)

(* Student Huffman *)
module H = Huffman

module HSol = struct
  type bit = H.bit
  type 'a hufftree = 'a H.hufftree
  type 'a encoding = 'a H.encoding

  module PQ = PQueue.ListImpl

	(** increment the value of x in the association list a or insert it. *)
	let rec incr a x = match a with
	  | []                   -> [x, 1]
	  | (y,n)::tl when x = y -> (x, n + 1)::tl
	  | hd::tl               -> hd::incr tl x

	(** return an association list mapping characters to counts in the input *)
	let rec collect l = List.fold_left incr [] l

	(** Compare worklist entries based on frequency *)
	let compare (t1, n1) (t2, n2) = if n1 < n2 then Gt
	                                else if n1 = n2 then Eq
	                                else Lt

	(* see mli *)
	let build_tree chars =
	  let counts   = collect chars in
	  let worklist = List.fold_left (fun q (c,n) -> PQ.insert (H.Leaf c, n) q)
	                                (PQ.empty compare)
	                                counts
	  in

	  let rec process worklist = match PQ.remove worklist with
	    | None -> None
	    | Some ((t, n), tl) -> match PQ.remove tl with
	      | None -> Some t
	      | Some ((t', n'), tl') -> process (PQ.insert (H.Node (t, t'), n + n') tl')
	  in
	  match process worklist with
	    | None          -> H.Empty
	    | Some t        -> H.Tree t

	(** return the encoding of a single character *)
	let find t c =
	  let rec find t k = match t with
	    | H.Leaf c' -> 
	      if c' = c then k (Some [])
	      else k None
	    | H.Node (l, r) ->
	      find l
	        (fun bo ->
	          match bo with
	            | Some bits -> k (Some (H.Zero::bits))
	            | None ->
	              find r (fun bo ->
	                match bo with
	                  | Some bits -> k (Some (H.One::bits))
	                  | None -> k None)) in
	  match find t (fun x -> x) with
	    | None      -> failwith "invalid encoding"
	    | Some bits -> bits

	(* see mli *)
	let encode t chars =
	  List.rev (List.fold_left (fun bits c -> List.rev_append (find t c) bits) [] chars)

	(* read a character from bits and return the character and the tail of bits *)
	let rec read_char t bits = match t, bits with
	  | Leaf c,      _          -> c, bits
	  | Node _,      []         -> failwith "not enough bits"
	  | Node (l, _), H.Zero::tl -> read_char l tl
	  | Node (_, r), H.One::tl  -> read_char r tl

	(* see mli *)
	let rec decode tree bits =
	  let rec helper bits acc = match bits with
	    | [] -> acc
	    | _  -> let c, tl = read_char tree bits in (helper tl (c::acc))
	  in List.rev (helper bits [])
end

let (===) x y = (Assertions.(===) x y); x=y

let rec tree_to_list = function
	| H.Leaf(v) -> [v]
	| H.Node(l, r) -> tree_to_list l @ tree_to_list r

let enc_to_list = function
	| H.Empty     -> []
	| H.Tree(tr1) -> tree_to_list tr1

let list_equals lst1 lst2 = 
  List.sort compare lst1 === List.sort compare lst2

let timed = Assertions.timeout

let timed_qc time cases test = Assertions.assert_qcheck cases (timed time test);
                               true

let random_chars l = 
	let rand_array = Array.init l (fun (x:int) -> (Char.chr (Random.int 125))) in 
	Array.to_list rand_array

let qcheck cases test = Assertions.assert_qcheck cases test; true

(* **************************************************************** *)

(** returns an association list with duplicate keys removed *)
let rec uniq chars = match chars with
  | []        -> []
  | (c,n)::tl -> if List.mem_assoc c chars then uniq tl else (c,n)::(uniq tl)

(** chars is an association list mapping characters to frequencies.  Produces a
    char list containing those frequencies *)
let rec mkstring chars = match chars with
  | []        -> []
  | (c,n)::tl -> (Array.to_list (Array.make n c)) @ (mkstring tl)

(** chars is an association list mapping characters to frequencies.  Produces
    the size of an encoded file using those frequencies. *)
let enc_size (enc : 'a H.encoding) (chars : ('a * int) list) : int =
  let rec helper n t = match t with
    | H.Leaf c     -> n * List.assoc c chars
    | H.Node (l,r) -> helper (succ n) l + helper (succ n) r
  in match enc with | H.Empty -> 0 | H.Tree t -> helper 0 t

(** returns true if student's tree contains correct characters *)
let test_correct chars = 
  let str   = mkstring chars in
  list_equals (enc_to_list (H.build_tree str))
              (List.map fst chars)

(** returns true if student's tree is optimal. *)
let test_opt chars =
  let str = mkstring chars in
  let stu = H.build_tree str in
  let sol = HSol.build_tree str in
  enc_size stu chars = enc_size sol chars

(** returns an arbitrary list of (character, count) pairs *)
let arb_chars = Arbitrary.(
  list (pair alpha (1 -- 15)) >>= fun l -> return (uniq l)
)

TEST "build_tree_has_correct_mappings" = timed_qc 5 arb_chars test_correct
                                      && (H.build_tree [] = H.Empty)

TEST "build_tree_is_optimal"           = timed_qc 5 arb_chars test_opt


(** an arbitrary tree using the characters in [chars] *)
let rec arb_tree chars random =
  (** inserts x in a random position in l. *)
  let insert x l =
    let a,b = List.partition (fun _ -> Arbitrary.bool random) l in
    a@[x]@b
  in
  let rec build_tree wl = match wl with
    | []       -> failwith "Test harness is broken, please submit a regrade request"
    | [x]      -> x
    | x::y::tl -> build_tree (insert (H.Node (x,y)) tl)
  in

  build_tree (List.map (fun x -> Leaf x) chars)


(** an arbitrary tree and a string that matches that tree *)
let arb_tree_and_string = Arbitrary.(
  arb_chars >>= fun pairs ->
  let chars = List.map fst pairs in
  pair (arb_tree chars) (list (among chars))
)

let test_encode (t,s) =
  H.encode t s = HSol.encode t s

let test_decode (t,s) =
  H.decode t (HSol.encode t s) = s

let test_encode_with_self s =
  match H.build_tree s with
    | Empty         -> true
    | Tree (Leaf c) -> true
    | Tree t        -> test_encode (t,s)

let test_decode_with_self s =
  match H.build_tree s with
    | Empty         -> true
    | Tree (Leaf c) -> true
    | Tree t        -> test_decode (t,s)

(** does encode return the right thing when given a random tree? *)
TEST "encode_correct" = timed_qc 5 arb_tree_and_string test_encode

(** does decode return the right thing when given a random tree? *)
TEST "decode_correct" = timed_qc 5 arb_tree_and_string test_decode

(** does encode work when given a tree generated by build_tree? *)
TEST "partial_credit_encode_correct_with_self" =
  timed_qc 5 Arbitrary.(list alpha ~len:(10--100)) test_encode_with_self

(** does decode work when given a tree generated by build_tree? *)
TEST "partial_credit_decode_correct_with_self" =
  timed_qc 5 Arbitrary.(list alpha ~len:(10--100)) test_decode_with_self


let round_trip s =
  match H.build_tree s with
    | Empty  -> false
    | Tree t -> s = H.decode t (H.encode t s)

(** does encoding and then decoding give the same result? *)
TEST "partial_credit_round_trip" =
  timed_qc 5 Arbitrary.(list alpha ~len:(10--100)) round_trip

let stack_overflow_test () = 
	let compress (lst : 'a list) = 
		let tree = (H.build_tree lst) in 
		match tree with 
		|H.Empty -> false
		|H.Tree(tr1) -> begin
			let x = H.encode tr1 lst in 
			if (List.length x) > 0 then true
			else false
		end in   
	timed 12 compress Arbitrary.(list_repeat 20000 alpha (Random.get_state ()))

TEST "stack_overflow" = stack_overflow_test ()	

