open Ast
open Eval
open Assertions

let option_spec  = Parser.parse_variant_spec "type 'a option = Some of 'a | None of unit"

let list_spec  = Parser.parse_variant_spec "type 'a list = Nil of unit | Cons of ('a * 'a list)"

(**************** tests ***************)

(** Constants, binary operators, pairs, and constructors *)
  let level1_noerr = [
    "17";
    "true";
    "1729+5040";
    "(1*17)>5";
    "\"hello\"";
    "\"hello\"^\"world\"";
    "()";
    "(2, 3)";
    "(false, true)";
    "(true, ())";
    "(\"hi\", (3+7))";
    "(3, (9+9, 12*7))";
    "((\"hello\", \"hi\"^\"world\"), 12*7)";
    "((true, ()), (9+9, 12*7))";
    "Nil ()";
    "Cons(1, Nil ())";
    "Cons(false, Cons(true, Nil()))";
  ]

  let level1_sol : Eval.value list = [
    VInt 17;
    VBool true;
    VInt 6769;
    VBool true;
    VString "hello";
    VString "helloworld";
    VUnit;
    VPair (VInt 2, VInt 3);
    VPair (VBool false, VBool true);
    VPair (VBool true, VUnit);
    VPair (VString "hi", VInt 10);
    VPair (VInt 3, VPair (VInt 18, VInt 84));
    VPair (VPair (VString "hello", VString "hiworld"), VInt 84);
    VPair (VPair (VBool true, VUnit), VPair (VInt 18, VInt 84));
    VVariant ("Nil", VUnit);
    VVariant ("Cons", VPair (VInt 1, VVariant ("Nil", VUnit)));
    VVariant ("Cons", VPair (VBool false, VVariant ("Cons",
      VPair (VBool true, VVariant ("Nil", VUnit)))));
  ]

  let level1_err = [
    "1 - false";
    "3 ^ ()";
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
    "if 0 * 42 = 0 then
      let x = 7 in x*6 else let y = 6 in y*6";
    "let x = (1, \"zardoz\") in x";
    "let x = (1, (2, (((), true), \"3110\"))) in x";
    "let x = None () in Some x";
  ]

  let level2_sol : Eval.value list = [
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
    VPair (VInt 1, VString "zardoz");
    VPair (VInt 1, VPair (VInt 2, VPair(VPair (VUnit, VBool true),
        VString "3110")));
    VVariant ("Some", VVariant ("None", VUnit));
  ]

  let level2_err = [
    "if 3 + 3 then \"hi\" else \"bye\"";
    "let x = 5 in y + x";
  ]

(** Higher order functions, no polymorphism *)
  let level3_noerr = [
    "fun n -> n+1";
    "(fun b -> fun c -> if b = c then true else false) 1 1";
    "let x = 7 in fun y -> x+y";
    "let x = 7 in (fun x -> x+x) 17";
    "fun n -> fun m -> n < m";
    "(fun x -> fun y -> let x = 7 in x + (x * y)) 4 5";
    "(fun x -> fun y -> fun z -> x (y+z) + 1) (fun x -> x+1) 4 5";
    "let f = fun x -> x - (1 * x) in (fun f -> f 42) f";
    "let f = fun g -> fun h -> fun x -> g ((h (x+1)) + 1) + 1 in
      f (fun x -> x+1) (fun x -> x-1) 42";
    "(fun f -> fun g -> fun x -> f (g x) + g (f x)) (fun x -> x*x)
      (fun x -> x+x) 3";
  ]

  let level3_sol = [
    VClosure ("n", BinOp (Plus, Var "n", Int 1), []);
    VBool true;
    VClosure ("y", BinOp (Plus, Var "x", Var "y"), ["x", ref (VInt 7)]);
    VInt 34;
    VClosure ("n", Parser.parse_expr "fun m -> n < m", []);
    VInt 42;
    VInt 11;
    VInt 0;
    VInt 45;
    VInt 54;
  ]

(** Matching and recursion, no polymorphism *)
  let level4_noerr = [
    "(fun x -> match x with true -> false | false -> true) false";
    "let rec fact = fun n -> if n=0 then 1 else n * fact (n-1) in fact 5";
    "match Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil ()))))) with
      Cons (a, Cons (b, Cons (c, Cons (d, Cons (e, Nil ()))))) ->
        a + b + c + d";
    "let rec evil = fun f -> fun g -> fun n ->
      let f' = fun x -> 10 + n in
      if n=1 then f' 0 + f 0 + g 0 else evil f' f (n-1) in
      let dummy = fun x -> 1000 in
      evil dummy dummy 3";
    "match 1729 with x -> x+1";
    "match Cons (1, Nil()) with Nil () -> false | Cons (x, xs) -> true";
    "let rec fib = fun n -> if n < 2 then n else fib (n-1) + fib (n-2) in
      fib 5";
    "let rec add = fun n -> fun m -> if n=0 then m else add (n-1) (m+1) in
      add 0 42";
    "let is_empty = fun xs -> match xs with Nil () -> true | _ -> false in
     is_empty (Nil ())";
    "let has_two = fun xs -> match xs with Nil () -> false | Cons (x, xs) ->
      match xs with Nil () -> false | _ -> true in
      has_two (Cons (1, Cons (2, Nil ())))";
    "let swag = let questionable = (fun x -> match x with Cons (h, Nil ()) ->
      h) in questionable Cons (42, Nil ()) in swag"
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

  (** (Non-recursive) Functions with polymorphism *)
  let level5_noerr = [
    "(fun x -> x)";
    "fun x -> fun y -> x";
    "(fun x -> fun y -> fun z -> x z (y z))";
    "let flip = fun f -> fun x -> fun y -> f y x in flip";
    "(fun x -> fun k -> k x)";
    "let bind = fun m -> fun f -> fun k -> m (fun v -> f v k) in
     bind ((fun x -> fun k -> k x) 3) (fun v -> fun k -> k (v+1)) (fun x -> x)";
    "let compose = fun f -> fun g -> fun x -> f (g x) in
      compose (fun x -> x+x) (fun x -> x*x) 3";
    "(fun x -> fun y -> fun z -> if x y then z else y z) (fun x -> true)
      (fun x -> 42) 24";
    "(fun x -> fun y -> x y y) (fun x -> fun y -> y) 42";
    "let a = fun x -> fun y -> fun k -> k (x+y) in
     let s = fun x -> fun k -> k (x*x) in
     let n = fun x -> fun y -> fun k -> s x (fun v -> s y (fun u ->
       a u v (fun s -> k s))) in
     n 3 4 (fun x -> x)"
  ]

  let level5_sol : Eval.value list =[
    VClosure ("x", Var "x", []);
    VClosure ("x", Fun ("y", Var "x"), []);
    VClosure ("x", Parser.parse_expr "fun y -> fun z -> x z (y z)", []);
    VClosure ("f", Parser.parse_expr "fun x -> fun y -> f y x", []);
    VClosure ("x", Parser.parse_expr "fun k -> k x", []);
    VInt 4;
    VInt 18;
    VInt 24;
    VInt 42;
    VInt 25;
  ]

(** Recursive functions with polymorphism *)
  let level6_noerr = [
    "let rec fix = fun f -> fun x -> f (fix f) x in
     fix (fun f -> fun n -> if n=0 then 1 else n * f(n-1)) 5";
    "let rec ntimes = fun n -> fun f -> fun x -> if n=0 then x else
      f (ntimes (n-1) f x) in ntimes 42 (fun x -> x+1) 0";
    "let rec alt = fun n -> fun f -> fun g -> fun x ->
     if n=0 then x else
     if n = 2 then f (alt (n-1) f g x)
     else if n = 4 then f (alt (n-1) f g x)
     else g (alt (n-1) f g x) in
     alt 5 (fun x -> x+1) (fun x -> x-1) 43";
  ]

  let level6_sol : Eval.value list = [
    VInt 120;
    VInt 42;
    VInt 42;
  ]

(** Complex Match Cases*)
  let level7_noerr = [
  "let rec uniq = fun xs -> match xs with
   | Nil () -> Nil ()
   | Cons (x, Nil ()) -> Cons (x, Nil ())
   | Cons (h, Cons (ht, tl)) -> if h=ht then uniq (Cons (h, tl))
                                else Cons (h, uniq (Cons (ht, tl))) in
   uniq (Cons (1, Cons (1, Cons (13, Cons (13, Cons (13, Cons (17,
     Cons(17, Nil ()))))))))";
  "let rec by_three = fun f -> fun xs -> match xs with
   | Cons (h, Cons (ht, Cons (htt, t))) -> Cons ((f h ht htt), (by_three f t))
   | x -> x in
  by_three (fun x -> fun y -> fun z -> x+y+z)
    (Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Cons (6, Nil ())))))))";
  "match (Cons (1, Cons (2, Cons (3, Nil ())))) with
   | Cons (x, Cons (y, Cons (z, Nil ()))) ->
       (match z with 3 -> (y > x) | _ -> false)
   | _ -> false";
  "match (Cons (Some true, Cons (None (), Nil ()))) with
  | Cons (Some x, y) -> 17
  | Cons (None (), x) -> 3110
  | Nil () -> 42";
  ]

  let level7_sol : Eval.value list = [
    VVariant ("Cons", VPair (VInt 1, VVariant ("Cons", VPair (VInt 13,
      VVariant ("Cons", VPair (VInt 17, VVariant ("Nil", VUnit)))))));
    VVariant ("Cons", VPair (VInt 6, VVariant ("Cons", VPair (VInt 15,
      VVariant ("Nil", VUnit)))));
    VBool true;
    VInt 17;
  ]

(** Hard Test Cases *)
  let level8_noerr = [
    "let rec fact_cps = fun n -> fun k ->
     if n=0 then k 1 else fact_cps (n-1) (fun x -> k (x*n)) in fact_cps 5 (fun x -> x)";
    "let dfs = fun t ->
       let rec append = fun x -> fun y -> fun k -> match x with
       | Nil () -> k y
       | Cons (h, t) -> append t y (fun z -> Cons (h, z)) in
       let a = fun x -> fun y -> append x y (fun x -> x) in
     t (fun l -> fun x -> fun r -> Cons (x, (a l r))) (Nil ()) in
     dfs (fun f -> fun i ->
       let l = f i 1 i in
       let r = f i 3 i in
       f l 2 r)";
    "let rec sort =
       let rec filter = fun f -> fun xs -> match xs with
       | Nil () -> Nil ()
       | Cons (x, xs) -> if f x then Cons (x, (filter f xs))
                         else filter f xs in
       let rec append = fun x -> fun y -> fun k -> match x with
       | Nil () -> k y
       | Cons (h, t) -> append t y (fun z -> Cons (h, z)) in
       let a = fun x -> fun y -> append x y (fun x -> x) in
       fun l -> match l with
       | Nil ()       -> Nil ()
       | Cons (x, xs) ->
         let left = filter (fun n -> n <= x) xs in
         let right = filter (fun n -> n > x) xs in
         a (sort left) (Cons (x, sort right)) in
       sort (Cons (2, Cons (1, Cons (3, Nil ()))))";
  ]

  let level8_sol : Eval.value list = [
    VInt 120;
    VVariant ("Cons", VPair (VInt 2, VVariant ("Cons", VPair (VInt 1,
      VVariant ("Cons", VPair (VInt 3, VVariant ("Nil", VUnit)))))));
    VVariant ("Cons", VPair (VInt 1, VVariant ("Cons", VPair (VInt 2,
      VVariant ("Cons", VPair (VInt 3, VVariant ("Nil", VUnit)))))));
  ]


let levels = [
  level1_noerr;
  level2_noerr;
  level3_noerr;
  level4_noerr;
  level5_noerr;
  level6_noerr;
  level7_noerr;
  level8_noerr;
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
]

let rec format_value f = function
  | VUnit          -> Format.fprintf f "()"
  | VInt n         -> Format.fprintf f "%i" n
  | VBool b        -> Format.fprintf f "%b" b
  | VString s      -> Format.fprintf f "\"%s\"" s
  | VClosure _     -> Format.fprintf f "<fun>"
  | VVariant (c,v) -> Format.fprintf f "%s (%a)" c format_value v
  | VPair (v1,v2)  -> Format.fprintf f "(%a,%a)" format_value v1 format_value v2
  | VError s       -> Format.fprintf f "%s" s

let print_value     = Printer.make_printer format_value
let string_of_value = Printer.make_string_of format_value

let timed f = Assertions.timeout 2 f ()

exception Test_failed of string
let run_test i j =
  let test_str = List.nth  (List.nth levels (i-1)) j in
  let test = Parser.parse_expr test_str in
  timed begin fun () ->
    let student_val = Eval.eval ([]) test in
    let solution_val = List.nth  (List.nth  solutions (i-1)) j in
    match student_val = solution_val with
    | true -> ()
    | false -> begin
      let stu_str = string_of_value student_val in
      let sol_str = string_of_value solution_val in
      raise (Test_failed (Format.sprintf "Test: %s Got '%s', expected '%s'"
        test_str stu_str sol_str))
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
TEST_UNIT "eval-1-11" = run_test 1 10
TEST_UNIT "eval-1-12" = run_test 1 11
TEST_UNIT "eval-1-13" = run_test 1 12
TEST_UNIT "eval-1-14" = run_test 1 13
TEST_UNIT "eval-1-15" = run_test 1 14
TEST_UNIT "eval-1-16" = run_test 1 15
TEST_UNIT "eval-1-17" = run_test 1 16
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
TEST_UNIT "eval-2-11" = run_test 2 10
TEST_UNIT "eval-2-12" = run_test 2 11
TEST_UNIT "eval-2-13" = run_test 2 12
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
TEST_UNIT "eval-4-11" = run_test 4 10
TEST_UNIT "eval-5-1" = run_test 5 0
TEST_UNIT "eval-5-2" = run_test 5 1
TEST_UNIT "eval-5-3" = run_test 5 2
TEST_UNIT "eval-5-4" = run_test 5 3
TEST_UNIT "eval-5-5" = run_test 5 4
TEST_UNIT "eval-5-6" = run_test 5 5
TEST_UNIT "eval-5-7" = run_test 5 6
TEST_UNIT "eval-5-8" = run_test 5 7
TEST_UNIT "eval-5-9" = run_test 5 8
TEST_UNIT "eval-5-10" = run_test 5 9
TEST_UNIT "eval-6-1" = run_test 6 0
TEST_UNIT "eval-6-2" = run_test 6 1
TEST_UNIT "eval-6-3" = run_test 6 2
(*TEST_UNIT "eval-6-4" = run_test 6 3
TEST_UNIT "eval-6-5" = run_test 6 4
TEST_UNIT "eval-6-6" = run_test 6 5
TEST_UNIT "eval-6-7" = run_test 6 6
TEST_UNIT "eval-6-8" = run_test 6 7
TEST_UNIT "eval-6-9" = run_test 6 8
TEST_UNIT "eval-6-10" = run_test 6 9*)
TEST_UNIT "eval-7-1" = run_test 7 0
TEST_UNIT "eval-7-2" = run_test 7 1
TEST_UNIT "eval-7-3" = run_test 7 2
TEST_UNIT "eval-7-4" = run_test 7 3
(* TEST_UNIT "eval-7-5" = run_test 7 4 *)
(* TEST_UNIT "eval-7-6" = run_test 7 5 *)
(* TEST_UNIT "eval-7-7" = run_test 7 6 *)
(* TEST_UNIT "eval-7-8" = run_test 7 7 *)
(* TEST_UNIT "eval-7-10" = run_test 7 9 *)
TEST_UNIT "eval-8-1" = run_test 8 0
TEST_UNIT "eval-8-2" = run_test 8 1
TEST_UNIT "eval-8-3" = run_test 8 2
(*TEST_UNIT "eval-8-4" = run_test 8 3
TEST_UNIT "eval-8-5" = run_test 8 4
TEST_UNIT "eval-8-6" = run_test 8 5
TEST_UNIT "eval-8-7" = run_test 8 6 *)
(* TEST_UNIT "eval-8-8" = run_test 8 7 *)
(* TEST_UNIT "eval-8-10" = run_test 8 9 *)
(*TEST_UNIT "eval-9-1" = run_test 9 0
TEST_UNIT "eval-9-2" = run_test 9 1
TEST_UNIT "eval-9-3" = run_test 9 2*)
(* TEST_UNIT "eval-9-4" = run_test 9 3 *)
(* TEST_UNIT "eval-9-5" = run_test 9 4 *)
(* TEST_UNIT "eval-9-6" = run_test 9 5 *)
(* TEST_UNIT "eval-9-7" = run_test 9 6 *)
(* TEST_UNIT "eval-9-8" = run_test 9 7 *)
(* TEST_UNIT "eval-9-10" = run_test 9 9 *)
(*TEST_UNIT "eval-10-1" = run_test 10 0
TEST_UNIT "eval-10-2" = run_test 10 1
TEST_UNIT "eval-10-3" = run_test 10 2*)
(* TEST_UNIT "eval-10-4" = run_test 10 3 *)
(* TEST_UNIT "eval-10-5" = run_test 10 4 *)
(* TEST_UNIT "eval-10-6" = run_test 10 5 *)
(* TEST_UNIT "eval-10-7" = run_test 10 6 *)
(* TEST_UNIT "eval-10-8" = run_test 10 7 *)
(* TEST_UNIT "eval-10-9" = run_test 10 8 *)
(* TEST_UNIT "eval-10-10" = run_test 10 9 *)
