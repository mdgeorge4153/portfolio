
open ReleaseTypes

let f0 f = 0, function []      -> f       | _ -> failwith "oops"
let f1 f = 1, function [x]     -> f x     | _ -> failwith "oops"
let f2 f = 2, function [x;y]   -> f x y   | _ -> failwith "oops"
let f3 f = 3, function [x;y;z] -> f x y z | _ -> failwith "oops"

(* returns a list containing all possible lists of n elements from nums *)
let make_tests nums n =
  let rec helper i =
    if i = 0
    then [[]]
    else
      let smaller = helper (pred i) in
      List.flatten (List.map (fun n ->
          List.map (fun l -> n :: l) smaller
        ) nums
      )
  in helper n

let run_tests tests nums =
  let inputs = List.map (make_tests nums) [0; 1; 2; 3] in

  (* returns a list of failing inputs for the given test *)
  let run_test ((args, test) : int * ('a list -> bool)) =
    let tests = List.nth tests args in
    List.filter (fun input -> not (test input)) (List.nth inputs args)
  in

  let results = List.map (fun (name, test) -> name, run_test test) tests in
  let results = List.filter (fun (_, res) -> List.length res > 0) results in

  results

let rec format_list format out l = match l with
  | []    -> ()
  | [x]   -> format out x
  | x::xs -> Format.fprintf out "%a, %a" format x (format_list format) xs

let format_results format results =
  let rec helper l = match l with
    | [] -> Format.printf "\n"
    | (prop_name, examples)::xs ->
        Format.printf "failed test %s on inputs " prop_name;
        let args = format_list format in
        begin match examples with
        | [x]           -> Format.printf "(%a)" args x
        | [x1;x2]       -> Format.printf "(%a) and (%a)" args x1 args x2
        | [x1;x2;x3]    -> Format.printf "(%a), (%a) and (%a)" args x1 args x2 args x3
        | x1::x2::x3::_ -> Format.printf "(%a), (%a), (%a) and others" args x1 args x2 args x3
        end;
        Format.printf "\n";
        helper xs
  in match results with
    | [] -> Format.printf "all tests pass\n\n"
    | _  -> helper results

module RingTests (R : NiceRing) = struct
  module Props = OrderedRingProperties(R)
  open Props

  let props = [
    "symmetric",            f2 symmetric;
    "transitive",           f3 transitive;
    "reflexive",            f1 reflexive;
    "commutative",          f2 commutative;
    "associative",          f3 associative;
    "identity",             f1 identity;
    "inverses",             f1 inverses;
    "times_associative",    f3 times_associative;
    "times_distributive",   f3 times_distributive;
    "times_identity",       f1 times_identity;
    "times_commutative",    f2 times_commutative;
    "minus_on_negative",    f0 minus_one_negative;
    "squares_non_negative", f1 squares_non_negative;
    "non_negative_times",   f2 non_negative_times;
    "non_negative_plus",    f2 non_negative_plus;
  ]

  open R

  let two = one + one
  let nums = [zero; one; -one; two; -two]

  let test ps ns =
    let results = run_tests (props @ ps) (nums @ ns) in
    format_results format results

end

let test_identities_root23 () = 
  let two = Numbers.Root23.( * )  Numbers.Root23.sqrt2 Numbers.Root23.sqrt2 in
  let _ = if Numbers.Root23.(===) two ( Numbers.Root23.( + ) Numbers.Root23.one  Numbers.Root23.one) then
         () else print_endline "Fail sqrt2 + sqrt2" in 
  let three = Numbers.Root23.( * )  Numbers.Root23.sqrt3 Numbers.Root23.sqrt3 in
  let _ = if Numbers.Root23.(===) three ( Numbers.Root23.( + )( Numbers.Root23.( + ) Numbers.Root23.one Numbers.Root23.one) 
                     Numbers.Root23.one) then
         () else print_endline "Fail sqrt3 + sqrt3" in 
  ()


let test_identities_rot15 () =
  let test_sin45 = Numbers.Rot15.( * ) Numbers.Rot15.sin45 Numbers.Rot15.sin45 in
  let test_cos45 = Numbers.Rot15.( * ) Numbers.Rot15.cos45 Numbers.Rot15.cos45 in
  let test_sin30 = Numbers.Rot15.( * ) Numbers.Rot15.sin30 Numbers.Rot15.sin30 in
  let test_cos30 = Numbers.Rot15.( * ) Numbers.Rot15.cos30 Numbers.Rot15.cos30 in
  let one        = Numbers.Rot15.one in
  let two        = Numbers.Rot15.(+) Numbers.Rot15.one Numbers.Rot15.one in
  let half       = Numbers.Rot15.inv two in
  let one4th     = Numbers.Rot15.inv (Numbers.Rot15.(+) two two) in
  let three4ths  = Numbers.Rot15.(+) half one4th in
  let (==)       = Numbers.Rot15.(===) in
  let _ = if test_sin45 == half then () else print_endline "Fail sin45" in
  let _ = if test_cos45 == half then () else print_endline "Fail cos45" in
  let _ = if test_sin30 == one4th then () else print_endline "Fail sin30" in
  let _ = if test_cos30 == three4ths then () else print_endline "Fail cos30" in
()

    

module FieldTests (F : NiceField) = struct
  module RT = RingTests(F)
  open RT

  module FProps = OrderedFieldProperties(F)
  open FProps

  open F


  let props = props @ [
    "times_inverse", f1 times_inverse;
  ]

  let nums  = nums @ [inv two; -inv two]

  let test ps ns = test (props @ ps) (nums @ ns)
end

module TestInts = RingTests(Numbers.Ints)
module IntUtils = NumberUtils.OrderedRingUtils(Numbers.Ints)
module TestIntegers = RingTests(Numbers.Integers)
module IntegerUtils = NumberUtils.OrderedRingUtils(Numbers.Integers)
module TestFloats = FieldTests(Numbers.Floats)
module FloatUtils = NumberUtils.OrderedFieldUtils(Numbers.Floats)
module TestRoot23 = RingTests(Numbers.Root23)
module Root23Utils = NumberUtils.OrderedRingUtils(Numbers.Root23)
module TestRot15 = FieldTests(Numbers.Rot15)
module Rot15Utils = NumberUtils.OrderedFieldUtils(Numbers.Rot15)
module TestRationals = FieldTests(Numbers.Rationals)
module RationalsUtils = NumberUtils.OrderedFieldUtils(Numbers.Rationals)

let _ = print_string "--- Ints Testing --- \n" 
let n_of_i = IntUtils.number_of_int
let () = TestInts.test [] []

let _ = print_string "-- Following ints tests should fail -- \n"  
let () = TestInts.test [] [n_of_i max_int]

let _ = print_string "--- Integers Testing --- \n" 
let n_of_i = IntegerUtils.number_of_int
let () = TestIntegers.test [] [n_of_i max_int; n_of_i (-max_int)]
let _ = print_string "-- No integers tests should fail -- \n" 

let _ = print_string "--- Floats Testing --- \n "
let n_of_i = FloatUtils.number_of_int
let () = TestFloats.test [] []
let _ = print_string "--- Floats Testing the following should fail --- \n " 
let e7 = Numbers.Floats.inv (n_of_i 100000)
let () = TestFloats.test [] [e7]


let _ = print_string "--- Root23 Testing --- \n" 
let n_of_i = Root23Utils.number_of_int
let () = TestRoot23.test [] []
let _ = print_string "--- Root23 Identity Testing --- \n"
let _ = test_identities_root23 ()


let _ = print_string "--- Rot15 Testing --- \n"
let n_of_i = Rot15Utils.number_of_int
let () = TestRot15.test [] [n_of_i max_int; n_of_i (-max_int)]
let _ = print_string "--- Rot15 Identity Testing --- \n"
let _ = test_identities_rot15 ()


let _ = print_string "--- Rationals Testing --- \n"
let n_of_i = RationalsUtils.number_of_int
let () = TestRationals.test [] [n_of_i max_int; n_of_i (-max_int)]




(*
** vim: ts=2 sw=2 ai et
*)


(*
** vim: ts=2 sw=2 ai et
*)
