open Ast
open Eval
open Assertions

module Eval_gauntlet = struct
  open Ast
  open Repl

(** Constants and binary operators *)
  let level1_noerr = [
    "17";
    "true";
    "1729+5040";
    "[] > [1]";
    "true && false";
    "(1*17)>5 || 3 < 2";
    "(1/2) mod 42";
    "not (1=1)";
    "[[1]]";
    "(42 - 3 mod 3) <= 42 && (true <= true)";
  ]

  let level1_sol : Ast.value list = [
    VInt 17;
    VBool true;
    VInt 6769;
    VBool false;
    VBool false;
    VBool true;
    VInt 0;
    VBool false;
    VCons (VCons (VInt 1, VNil), VNil);
    VBool true;
  ]

  let level1_err = [
    "3 || true";
    "1 - false";
  ]

(** If and Let expressions with no polymorphism. *)
  let level2_noerr = [
    "if true then 42 else 0";
    "if 1=1 then false else true";
    "if 17>5040 then 1 else (1-1)*(1+1)";
    "let x = 3 in x";
    "let x = 3 in let y = 4 in x+y";
    "let x = 7 in let x = 17 in x > 16";
    "let x = 3 in if x=4 then x-1 else x*x";
    "if true then let x = 3 in x+39 else let x = 7 in x+10";
    "if false then 0 else let x = 41 in if x < 50 then let y = 1 in x+y else 0";
      "if 0 mod 42 = 0 then
   let x = 7 in x*6 else let y = 6 in y*6";
  ]

  let level2_sol : Ast.value list = [
    VInt 42;
    VBool false;
    VInt 0;
    VInt 3;
    VInt 7;
    VBool true;
    VInt 9;
    VInt 42;
    VInt 42;
    VInt 42;
  ]

(** Higher order functions, no polymorphism *)
  let level3_noerr = [
    "fun n -> n+1";
    "(fun b c -> if b = c then true else false) false false";
    "let x = 7 in fun y -> x+y";
    "let x = 7 in (fun x -> x+x) 17";
    "fun n m -> n < m";
    "(fun x y -> let x = 7 in x + (x * y)) 4 5";
    "(fun x y z -> x (y+z) + 1) (fun x -> x+1) 4 5";
    "let f x = x - (1 * x) in (fun f -> f 42) f";
    "let f g h x = g ((h (x+1)) + 1) + 1 in f (fun x -> x+1) (fun x -> x-1) 42";
    "(fun f g x -> f (g x) + g (f x)) (fun x -> x*x) (fun x -> x+x) 3";
  ]

  let level3_sol = [
    VClosure (IdMap.empty, "n", BinaryOp (Plus, Var "n", Constant(Int 1)));
    VBool true;
    VClosure (IdMap.singleton "x" (ref (VInt 7)), "y", BinaryOp (Plus, Var "x", Var "y"));
    VInt 34;
    VClosure (IdMap.empty, "n", parse_expression "fun m -> n < m");
    VInt 42;
    VInt 11;
    VInt 0;
    VInt 45;
    VInt 54;
  ]

(** Matching and recursion, no polymorphism *)
  let level4_noerr = [
    "(fun x -> match x with true -> false | false -> true) false";
    "let rec fact n = if n=0 then 1 else n * fact (n-1) in fact 5";
    "match [1;2;3;4;5] with a::b::c::d::e -> a + b + c + d";
    "let rec evil f g n =
    let f' x = 10 + n in
    if n=1 then f' 0 + f 0 + g 0 else evil f' f (n-1) in
   let dummy x = 1000 in
   evil dummy dummy 3";
    "match 1729 with x -> x+1";
    "match [1] with [] -> false | x::xs -> true";
    "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib 5";
    "let rec add n m = if n=0 then m else add (n-1) (m+1) in add 0 42";
    "let is_empty xs = match xs with [] -> true | _ -> false in is_empty []";
    "let has_two xs = match xs with [] -> false | x::xs -> match xs with [] -> false | _ -> true in has_two [1;2]";
    "let swag = let questionable = (fun x -> match x with [h] -> h) in questionable [42] in swag"
  ]

  let level4_sol = [
    VBool true;
    VInt 120;
    VInt 10;
    VInt 36;
    VInt 1730;
    VBool true;
    VInt 5;
    VInt 42;
    VBool true;
    VBool true;
    VInt 42;
  ]

  (** List processing with no polymorphism *)
  let level5_noerr = [
    "let rec succ_all xs = match xs with [] -> [] | x::xs -> (x+1)::(succ_all xs) in succ_all [1;2;3]";
    "let rec pred_all xs =
     match xs with [] -> [] | [1] -> [2] | x::xs -> (x-1)::pred_all xs in pred_all [3;2;1]";
    "let zip_with_index xs =
     let rec zipper xs n = match xs with
     | [] -> []
     | x::xs -> [x;n]::(zipper xs (n+1)) in
     zipper xs 0 in
     zip_with_index [1;2;3]";
    "let rec sum l seed = match l with [] -> seed | h::t -> h + (sum t seed) in let f = sum [1;2;3] in f 5 + f 3";
    "let rec nth n xs a =
       if n=0 then match xs with [] -> 1+a | h::_ -> h
       else match xs with [] -> 1-a | _::t -> nth (n-1) t a in
       nth 3 [1;2;3;4] 0";
    "let rec and_all xs =
       match xs with [] -> true | h::t -> h && and_all t in
       and_all [true; false; true]";
    "let rec filter_by f xs =
       match xs with
        [] -> []
      | h::t -> if f h then (h+1)::(filter_by f t) else filter_by f t in
      filter_by (fun x -> x > 17) [2;3;5;8]";
    "let rec permute_thing xs =
       match xs with
         [1;2;3] -> [2;1;3]
       | h::t -> permute_thing t
       | [] -> [] in
     permute_thing [4;1;2;3]";
    "let rec go_ham xs = match xs with
       [1;x] -> 20+x
     | h::t -> h+(go_ham t) in
     go_ham [1;2;3;1;16]";
    "let rec swag xs = match xs with x::y::z -> x (y 1) + swag z | _ -> 2 in
     swag [(fun x -> x+1); (fun x -> x*x)]"
  ]

  let level5_sol = [
    VCons (VInt 2, VCons (VInt 3, VCons (VInt 4, VNil)));
    VCons (VInt 2, VCons (VInt 1, VCons (VInt 2, VNil)));
    VCons (VCons (VInt 1, VCons (VInt 0, VNil)),
           VCons (VCons (VInt 2, VCons (VInt 1, VNil)),
                  VCons (VCons (VInt 3, VCons (VInt 2, VNil)), VNil)));
    VInt 20;
    VInt 4;
    VBool false;
    VNil;
    VCons (VInt 2, VCons (VInt 1, VCons (VInt 3, VNil)));
    VInt 42;
    VInt 4;
  ]

  (** (Non-recursive) Functions with polymorphism *)
  let level6_noerr = [
    "(fun x -> x)";
    "fun x y -> x";
    "(fun x y z -> x z (y z))";
    "let flip f x y = f y x in flip";
    "(fun x k -> k x)";
    "let bind m f = fun k -> m (fun v -> f v k) in
     bind ((fun x k -> k x) 3) (fun v k -> k (v+1)) (fun x -> x)";
    "let compose f g x = f (g x) in compose (fun x -> x+x) (fun x -> x*x) 3";
    "(fun x y z -> if x y then z else y z) (fun x -> true) (fun x -> 42) 24";
    "(fun x y -> x y y) (fun x y -> y) 42";
    "let a x y k = k (x+y) in
     let s x k = k (x*x) in
     let n x y k = s x (fun v -> s y (fun u -> a u v (fun s -> k s))) in
     n 3 4 (fun x -> x)"
  ]

  let level6_sol : Ast.value list =[
    VClosure (IdMap.empty, "x", Var "x");
    VClosure (IdMap.empty, "x", Fun ("y", Var "x"));
    VClosure (IdMap.empty, "x", parse_expression "fun y z -> x z (y z)");
    VClosure (IdMap.empty, "f", parse_expression "fun x y -> f y x");
    VClosure (IdMap.empty, "x", parse_expression "fun k -> k x");
    VInt 4;
    VInt 18;
    VInt 24;
    VInt 42;
    VInt 25;
  ]

(** Recursive functions with polymorphism *)
  let level7_noerr = [
    "let rec fix f x = f (fix f) x in
     fix (fun f n -> if n=0 then 1 else n * f(n-1)) 5";
    "let rec ntimes n f x = if n=0 then x else f (ntimes (n-1) f x) in ntimes 42 (fun x -> x+1) 0";
    "let rec alt n f g x =
     if n=0 then x else
     if n mod 2 = 0 then f (alt (n-1) f g x) else g (alt (n-1) f g x) in
     alt 5 (fun x -> x+1) (fun x -> x-1) 43";
  ]

  let level7_sol : Ast.value list = [
    VInt 120;
    VInt 42;
    VInt 42;
  ]

(** Lists with polymorphism *)
  let level8_noerr = [
  "let rec length xs = match xs with
   | [] -> 0
   | h::t -> 1 + length t in length [1;2;3;4;5]";
  "let rec map f xs = match xs with
   | [] -> []
   | h::t -> (f h) :: (map f t) in map (fun x -> x+2) [0;1]";
  "let rec foldl f a xs = match xs with [] -> a | x::xs -> foldl f (f a x) xs
   in foldl (fun a x -> x::a) [] [1;2;3]";
  "let rec filter f xs = match xs with
   | [] -> []
   | x::xs -> if f x then x::(filter f xs) else filter f xs in
   filter (fun x -> x > 17) [1;18;2]";
  "let rev_map f xs =
     let rec help xs a = match xs with
     | [] -> a
     | x::xs -> help xs ((f x)::a) in
     help xs [] in
     rev_map (fun x -> x*2) [1;2;3]";
  "let rec append xs ys = match xs with
   | [] -> ys
   | x::t -> x::(append t ys) in
   append [3] [2;1]";
  "let rec interleave x xs = match xs with
   | [] -> []
   | h::t -> x::h::(interleave x t) in
   interleave true [false;false]";
  ]

  let level8_sol : Ast.value list = [
    VInt 5;
    VCons (VInt 2, VCons (VInt 3, VNil));
    VCons (VInt 3, VCons (VInt 2, VCons (VInt 1, VNil)));
    VCons (VInt 18, VNil);
    VCons (VInt 6, VCons (VInt 4, VCons (VInt 2, VNil)));
    VCons (VInt 3, VCons (VInt 2, VCons (VInt 1, VNil)));
    VCons (VBool true, VCons (VBool false, VCons (VBool true, VCons (VBool false, VNil))));
  ]

(** Complex Match Cases*)
  let level9_noerr = [
    "let rec uniq xs = match xs with
   | [] -> []
   | [x] -> [x]
   | h::ht::tl -> if h=ht then uniq (h::tl) else h::(uniq (ht::tl)) in
   uniq [1;1;13;13;17;17]";
  "let rec by_three f xs = match xs with
   | h::ht::htt::t -> (f h ht htt)::(by_three f t)
   | x -> x in by_three (fun x y z -> x+y+z) [1;2;3;4;5;6]";
  "match [1;2;3] with
   | x::y::z -> (match z with [3] -> (y > x) && true | _ -> false)
   | _ -> false"
  ]

  let level9_sol : Ast.value list = [
    VCons (VInt 1, VCons (VInt 13, VCons (VInt 17, VNil)));
    VCons (VInt 6, VCons (VInt 15, VNil));
    VBool true;
  ]

(** Hard Test Cases *)
  let level10_noerr = [
    "let rec fact_cps n k =
     if n=0 then k 1 else fact_cps (n-1) (fun x -> k (x*n)) in fact_cps 5 (fun x -> x)";
    "let dfs t =
       let rec append x y k = match x with
       | [] -> k y
       | h::t -> append t y (fun z -> h::z) in
       let a x y = append x y (fun x -> x) in
     t (fun l x r -> x::(a l r)) [] in
     dfs (fun f i ->
       let l = f i 1 i in
       let r = f i 3 i in
       f l 2 r)";
    "let rec sort =
       let rec filter f xs = match xs with
       | [] -> []
       | x::xs -> if f x then x::(filter f xs) else filter f xs in
       let rec append x y k = match x with
       | [] -> k y
       | h::t -> append t y (fun z -> h::z) in
       let a x y = append x y (fun x -> x) in
       fun l -> match l with
       | []    -> []
       | x::xs -> 
         let left = filter (fun n -> n <= x) xs in
         let right = filter (fun n -> n > x) xs in
         a (sort left) (x :: sort right) in
       sort [2;1;3]";
  ]

  let level10_sol : Ast.value list = [
    VInt 120;
    VCons (VInt 2, VCons (VInt 1, VCons (VInt 3, VNil)));
    VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNil)));
  ]
end

open Repl
open Eval_gauntlet

let levels = [
  level1_noerr;
  level2_noerr;
  level3_noerr;
  level4_noerr;
  level5_noerr;
  level6_noerr;
  level7_noerr;
  level8_noerr;
  level9_noerr;
  level10_noerr;
]

let solutions = [
  level1_sol;
  level2_sol;
  level3_sol;
  level4_sol;
  level5_sol;
  level6_sol;
  level7_sol;
  level8_sol;
  level9_sol;
  level10_sol;
]

let timed f = Assertions.timeout 2 f ()

exception Test_failed of string
let run_test i j =
  let test_str = List.nth  (List.nth levels (i-1)) j in
  let test = parse_expression test_str in
  timed begin fun () ->
    let student_val = Eval.eval test (IdMap.empty) in
    let solution_val = List.nth  (List.nth  solutions (i-1)) j in
    match student_val = solution_val with
    | true -> ()
    | false -> begin
      let stu_str = Printer.val_to_string student_val in
      let sol_str = Printer.val_to_string solution_val in
      raise (Test_failed (Format.sprintf "Test: %s Got '%s', expected '%s'" test_str stu_str sol_str))
    end

  end

TEST_UNIT "eval-1-1" = run_test 1 0
TEST_UNIT "eval-1-2" = run_test 1 1
TEST_UNIT "eval-1-3" = run_test 1 2
TEST_UNIT "eval-1-4" = run_test 1 3
TEST_UNIT "eval-1-5" = run_test 1 4
TEST_UNIT "eval-1-6" = run_test 1 5
TEST_UNIT "eval-1-7" = run_test 1 6
TEST_UNIT "eval-1-8" = run_test 1 7
TEST_UNIT "eval-1-9" = run_test 1 8
TEST_UNIT "eval-1-10" = run_test 1 9
TEST_UNIT "eval-2-1" = run_test 2 0
TEST_UNIT "eval-2-2" = run_test 2 1
TEST_UNIT "eval-2-3" = run_test 2 2
TEST_UNIT "eval-2-4" = run_test 2 3
TEST_UNIT "eval-2-5" = run_test 2 4
TEST_UNIT "eval-2-6" = run_test 2 5
TEST_UNIT "eval-2-7" = run_test 2 6
TEST_UNIT "eval-2-8" = run_test 2 7
TEST_UNIT "eval-2-9" = run_test 2 8
TEST_UNIT "eval-2-10" = run_test 2 9
TEST_UNIT "eval-3-1" = run_test 3 0
TEST_UNIT "eval-3-2" = run_test 3 1
TEST_UNIT "eval-3-3" = run_test 3 2
TEST_UNIT "eval-3-4" = run_test 3 3
TEST_UNIT "eval-3-5" = run_test 3 4
TEST_UNIT "eval-3-6" = run_test 3 5
TEST_UNIT "eval-3-7" = run_test 3 6
TEST_UNIT "eval-3-8" = run_test 3 7
TEST_UNIT "eval-3-9" = run_test 3 8
TEST_UNIT "eval-3-10" = run_test 3 9
TEST_UNIT "eval-4-1" = run_test 4 0
TEST_UNIT "eval-4-2" = run_test 4 1
TEST_UNIT "eval-4-3" = run_test 4 2
TEST_UNIT "eval-4-4" = run_test 4 3
TEST_UNIT "eval-4-5" = run_test 4 4
TEST_UNIT "eval-4-6" = run_test 4 5
TEST_UNIT "eval-4-7" = run_test 4 6
TEST_UNIT "eval-4-8" = run_test 4 7
TEST_UNIT "eval-4-9" = run_test 4 8
TEST_UNIT "eval-4-10" = run_test 4 9
TEST_UNIT "eval-5-1" = run_test 5 0
TEST_UNIT "eval-5-2" = run_test 5 1
TEST_UNIT "eval-5-3" = run_test 5 2
TEST_UNIT "eval-5-4" = run_test 5 3
TEST_UNIT "eval-5-5" = run_test 5 4
TEST_UNIT "eval-5-6" = run_test 5 5
TEST_UNIT "eval-5-7" = run_test 5 6
TEST_UNIT "eval-5-8" = run_test 5 7
TEST_UNIT "eval-5-10" = run_test 5 9
TEST_UNIT "eval-6-1" = run_test 6 0
TEST_UNIT "eval-6-2" = run_test 6 1
TEST_UNIT "eval-6-3" = run_test 6 2
TEST_UNIT "eval-6-4" = run_test 6 3
TEST_UNIT "eval-6-5" = run_test 6 4
TEST_UNIT "eval-6-6" = run_test 6 5
TEST_UNIT "eval-6-7" = run_test 6 6
TEST_UNIT "eval-6-8" = run_test 6 7
TEST_UNIT "eval-6-9" = run_test 6 8
TEST_UNIT "eval-6-10" = run_test 6 9
TEST_UNIT "eval-7-1" = run_test 7 0
TEST_UNIT "eval-7-2" = run_test 7 1
TEST_UNIT "eval-7-3" = run_test 7 2
(* TEST_UNIT "eval-7-4" = run_test 7 3 *)
(* TEST_UNIT "eval-7-5" = run_test 7 4 *)
(* TEST_UNIT "eval-7-6" = run_test 7 5 *)
(* TEST_UNIT "eval-7-7" = run_test 7 6 *)
(* TEST_UNIT "eval-7-8" = run_test 7 7 *)
(* TEST_UNIT "eval-7-10" = run_test 7 9 *)
TEST_UNIT "eval-8-1" = run_test 8 0
TEST_UNIT "eval-8-2" = run_test 8 1
TEST_UNIT "eval-8-3" = run_test 8 2
TEST_UNIT "eval-8-4" = run_test 8 3
TEST_UNIT "eval-8-5" = run_test 8 4
TEST_UNIT "eval-8-6" = run_test 8 5
TEST_UNIT "eval-8-7" = run_test 8 6
(* TEST_UNIT "eval-8-8" = run_test 8 7 *)
(* TEST_UNIT "eval-8-10" = run_test 8 9 *)
TEST_UNIT "eval-9-1" = run_test 9 0
TEST_UNIT "eval-9-2" = run_test 9 1
TEST_UNIT "eval-9-3" = run_test 9 2
(* TEST_UNIT "eval-9-4" = run_test 9 3 *)
(* TEST_UNIT "eval-9-5" = run_test 9 4 *)
(* TEST_UNIT "eval-9-6" = run_test 9 5 *)
(* TEST_UNIT "eval-9-7" = run_test 9 6 *)
(* TEST_UNIT "eval-9-8" = run_test 9 7 *)
(* TEST_UNIT "eval-9-10" = run_test 9 9 *)
TEST_UNIT "eval-10-1" = run_test 10 0
TEST_UNIT "eval-10-2" = run_test 10 1
TEST_UNIT "eval-10-3" = run_test 10 2
(* TEST_UNIT "eval-10-4" = run_test 10 3 *)
(* TEST_UNIT "eval-10-5" = run_test 10 4 *)
(* TEST_UNIT "eval-10-6" = run_test 10 5 *)
(* TEST_UNIT "eval-10-7" = run_test 10 6 *)
(* TEST_UNIT "eval-10-8" = run_test 10 7 *)
(* TEST_UNIT "eval-10-9" = run_test 10 8 *)
(* TEST_UNIT "eval-10-10" = run_test 10 9 *)
