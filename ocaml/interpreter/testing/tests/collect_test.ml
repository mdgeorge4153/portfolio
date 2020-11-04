open Ast
open TypedAst
open Printer
open Parser
open Infer
open Eval
open Meta
open Assertions

let option_spec  = Parser.parse_variant_spec "type 'a option = Some of 'a | None of unit"

let list_spec  = Parser.parse_variant_spec "type 'a list = Nil of unit | Cons of ('a * 'a list)"

let rt_spec =
let typ_list = [TAlpha "a"; TAlpha "b"; TAlpha "c"] in
{vars = ["a"; "b"; "c"];
 name = "real_tree";
 constructors = [
  ("Leaf", TUnit);
  ("Branch", TStar(TAlpha "a", TVariant(typ_list,"real_tree")));
  ("Flower", TStar(TAlpha "b", TVariant(typ_list,"real_tree")));
  ("Fruit", TStar(TAlpha "c", TVariant(typ_list,"real_tree")));
  ("Jackpot", TStar(TAlpha "a", TStar (TAlpha "b", TAlpha "c")))
 ]
}
(* "type ('a,('b,'c)) real_tree =
  |Leaf of unit
  |Branch of ('a * ('a,('b,'c)) real_tree)
  |Flower of ('b * ('a,('b,'c)) real_tree)
  |Fruit of ('c * ('a,('b,'c)) real_tree)
  |Jackpot of ('a *('b *'c))" *)

let queue_spec = Parser.parse_variant_spec "type 'a queue = Queue of ('a list * 'a list)"

(**************** tests ***************)

(*testing normal*)
let rt1 = "Leaf ()"
let rt2 = "Branch (1, Leaf ())"
let rt3 = "Flower (\"hello\", Leaf ())"
let rt4 = "Fruit ((), Leaf ())"
let rt5 = "Jackpot (true,(1,()))"
(*testing nested varirants*)
let rt6 = "Branch (\"hello\", Flower((true,false),Leaf ()))"
let rt7 = "Flower ((true,1), Fruit((1,(2,3)),Leaf ()))"
let rt8 = "Fruit ((), Flower(2, Branch((true,(1,())),Leaf ())))"
let rt9 = "Fruit (true, Branch (1, Flower ((), Jackpot (2,((),false)))))"
(*testing polymorphism of variants*)
let rt10 = "(Flower (\"hello\", Leaf ()),Branch (\"hello\", Flower((true,false),Leaf ())))"
let rt11 = "(Jackpot (true,(1,())),Flower ((true,1), Fruit((1,(2,3)),Leaf ())))"
let rt12 = "(Leaf (),Branch (1, Leaf ()))"
let option_test  = "(Some 1, Some \"where\")"

(*testing errors for variants*)
let err_rt1 = "Branch (1, Branch(true,Leaf()))"
let err_rt2 = "Flower ((), Flower(1, Leaf()))"
let err_rt3 = "Fruit (true, Fruit(\"hello\",Leaf()))"
let err_rt4 = "Branch (1, Jackpot(true,((),\"hello\")))"
let err_rt5 = "Flower (true, Jackpot(1,(1,1)))"
let err_rt6 = "Fruit ((), Jackpot(true,(false,true)))"

let rt_sol_1 = TVariant([TAlpha "a"; TAlpha "b"; TAlpha "c"], "real_tree")
let rt_sol_2 = TVariant([TInt; TAlpha "a"; TAlpha "b"], "real_tree")
let rt_sol_3 = TVariant([TAlpha "a"; TString; TAlpha "b"], "real_tree")
let rt_sol_4 = TVariant([TAlpha "a"; TAlpha "b"; TUnit], "real_tree")
let rt_sol_5 = TVariant([TBool; TInt; TUnit], "real_tree")
let rt_sol_6 = TVariant([TString; TStar(TBool,TBool); TAlpha "a"], "real_tree")
let rt_sol_7 = TVariant([TAlpha "a"; TStar(TBool, TInt); TStar(TInt, TStar(TInt,TInt))], "real_tree")
let rt_sol_8 = TVariant([TStar(TBool,TStar(TInt,TUnit));TInt;TUnit], "real_tree")
let rt_sol_9 = TVariant([TInt; TUnit; TBool], "real_tree")
let rt_sol_10 = TStar(rt_sol_3, TVariant([TString; TStar(TBool,TBool); TAlpha "c"], "real_tree"))
let rt_sol_11 = TStar(rt_sol_5, rt_sol_7)
let rt_sol_12 = TStar(rt_sol_1, TVariant([TInt; TAlpha "d"; TAlpha "e"], "real_tree"))
let option_test_sol  = Parser.parse_type "int option * string option"

(*non polymorphic match statements*)
let rt_match_1 = "let random1 = fun rt -> match rt with |Branch(x,Flower(y,Fruit(z,Leaf()))) -> if x then y+z else 0 in random1"
let rt_match_2 = "let random2 = fun rt -> match rt with |Branch(x,_) -> x+1 in random2"
let rt_match_3 = "let random3 = fun rt -> match rt with |Flower(x,_) -> x^x in random3"
let rt_match_4 = "let random4 = fun rt -> match rt with |Fruit(x,_) -> if x then x else x in random4"
let rt_match_5 = "let random5 = fun rt -> match rt with |Flower(x,Fruit(y,_)) -> if y then x^x else x in random5"
let rt_match_6 = "let random6 = fun rt -> match rt with |Fruit(y,Branch(x,_)) -> if y then x else () in random6"
let rt_match_7 = "let random7 = fun rt -> match rt with |Jackpot(x,(y,z)) -> if z then y+y else x+x in random7"

let rt_match_1_sol = TArrow(TVariant([TBool; TInt; TInt], "real_tree"),TInt)
let rt_match_2_sol = TArrow(rt_sol_2, TInt)
let rt_match_3_sol = TArrow(rt_sol_3, TString)
let rt_match_4_sol = TArrow(TVariant([TAlpha "a"; TAlpha "b"; TBool], "real_tree"), TBool)
let rt_match_5_sol = TArrow(TVariant([TAlpha "a"; TString; TBool], "real_tree"), TString)
let rt_match_6_sol = TArrow(TVariant([TUnit; TAlpha "a"; TBool], "real_tree"), TUnit)
let rt_match_7_sol = TArrow(TVariant([TInt; TInt; TBool], "real_tree"),TInt)

(*polymorphic match statement*)
let rt_fold = "let rec fold_rt =
  fun lf -> fun f_br -> fun f_fl -> fun f_fr -> fun f_jp -> fun rt ->
  match rt with
  |Leaf () -> lf
  |Branch (x, rt') -> f_br x (fold_rt lf f_br f_fl f_fr f_jp rt')
  |Flower (x, rt') -> f_fl x (fold_rt lf f_br f_fl f_fr f_jp rt')
  |Fruit (x, rt') -> f_fr x (fold_rt lf f_br f_fl f_fr f_jp rt')
  |Jackpot (x, (y, z)) -> f_jp x y z lf in fold_rt"

let rt_fold_sol =
  let alpha = TAlpha "a" in
  let beta = TAlpha "b" in
  let gamma = TAlpha "c" in
  let delta = TAlpha "d" in
  let lf_type = TArrow(beta,TArrow(alpha,alpha)) in
  let fbr_type = TArrow(gamma,TArrow(alpha,alpha)) in
  let ffl_type = TArrow(delta,TArrow(alpha,alpha)) in
  let fjp_type = TArrow(beta,TArrow(gamma,TArrow(delta,TArrow(alpha,alpha))))in
  let rt_type = TVariant([beta;gamma;delta],"real_tree") in
  TArrow(alpha,TArrow(lf_type,TArrow(fbr_type,TArrow(ffl_type,TArrow(fjp_type,TArrow(rt_type,alpha))))))
   (* "'a -> ('b -> 'a -> 'a) -> ('c -> 'a -> 'a) -> *)
   (*      ('d -> 'a -> 'a) -> ('b -> 'c -> 'd -> 'a -> 'a) -> ('b, 'c, 'd) real_tree -> 'a" *)

let rem = "let rem = fun l ->
  match l with
  |Nil () -> None ()
  |Cons(h,t) -> Some (h,t) in rem"

let push = "let push = fun q -> fun x ->
  match q with
  |Queue(s1,s2) -> Queue(Cons(x,s1),s2) in push"

let transfer = "let rem = fun l ->
  match l with
    |Nil () -> None ()
    |Cons(h,t) -> Some (h,t) in
  let rec transfer = fun q ->
    match q with Queue(s1,s2) ->
      match rem s1 with
        |Some (h,t) -> transfer (Queue(t, Cons(h,s2)))
        |None () -> q in transfer"

let pop = "let rem = fun l ->
  match l with
    |Nil () -> None ()
    |Cons(h,t) -> Some (h,t) in
let rec transfer = fun q ->
  match q with Queue(s1,s2) ->
  match rem s1 with
  |Some (h,t) -> transfer (Queue(t, Cons(h,s2)))
  |None () -> q in
let pop = fun q ->
  match q with Queue(s1,s2) ->
  match rem s2 with
  |Some (h,s2') -> Some (h, Queue(s1,s2'))
  |None () ->
    match q with Queue(s1',s2') ->
    (match rem s2' with
     |Some (h,s2'') -> Some (h, Queue(s1,s2''))
     |None () -> None ()) in pop"

let rem_sol = Parser.parse_type "'a list -> ('a * 'a list) option"
let push_sol = Parser.parse_type "'a queue -> 'a -> 'a queue"
let transfer_sol = Parser.parse_type "'a queue -> 'a queue"
let pop_sol = Parser.parse_type "'a queue -> ('a * 'a queue) option"

(** Constants and binary operators *)
  let level1_noerr = [
    "17";
    "true";
    "1729+5040";
    "(1*17)>5";
    "\"hello\"";
    "\"hello\"^\"world\"";
    "()";
    "(2,3)";
    "(false,true)";
    "(true,())";
    "(\"hi\",(3+7))";
    "(3,(9+9,12*7))";
    "((\"hello\",\"hi\"^\"world\"),12*7)";
    "((true,()),(9+9,12*7))"
  ]

  let level1_sol = [
    TInt;
    TBool;
    TInt;
    TBool;
    TString;
    TString;
    TUnit;
    Parser.parse_type "int * int";
    Parser.parse_type "bool * bool";
    Parser.parse_type "bool * unit";
    Parser.parse_type "string * int";
    Parser.parse_type "int * (int * int)";
    Parser.parse_type "(string * string) * int";
    Parser.parse_type "(bool * unit) * (int * int)"
  ]

  let level1_err = [
    "3 || true";
    "1 - false";
    "3^()"
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
   "let x = (1,3) in x";
   "let x = (1,(2,(((),true),true))) in x"
  ]

  let level2_sol = [
    TInt;
    TBool;
    TInt;
    TInt;
    TInt;
    TBool;
    TInt;
    TInt;
    TInt;
    TInt;
    Parser.parse_type "int * int";
    Parser.parse_type "int * (int * ((unit * bool) * bool))"
  ]

  (** Higher order functions, no polymorphism *)
  let level3_noerr = [
    "fun n -> n+1";
    "(fun b -> fun c -> if b = c then true else false)";
    "let x = 7 in fun y -> x+y";
    "let x = 7 in (fun x -> x+x) 17";
    "(fun x -> fun y -> let x = 7 in x + (x * y)) true";
    "(fun x -> fun y -> fun z -> x (y+z) + 1)";
    "let f = fun x -> x - (1 * x) in (fun f -> f 42) f";
    "fun g -> fun h -> fun x -> g ((h (x+1)) + 1) + 1";
    "(fun f -> fun g -> fun x -> f (g x) + g (f x))";
  ]

  let level3_sol = [
    TArrow (TInt,TInt);
    TArrow (TInt, TArrow (TInt, TBool));
    TArrow (TInt, TInt);
    TInt;
    TArrow (TInt, TInt);
    TArrow (TArrow (TInt, TInt), TArrow (TInt, TArrow (TInt, TInt)));
    TInt;
    TArrow (TArrow (TInt, TInt), TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)));
    TArrow (TArrow (TInt, TInt), TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)));
  ]

  (** Matching and recursion, no polymorphism *)
  let level4_noerr = [
      "fun x -> match x with |(a,b) -> a+b |_ -> 1";
      "fun x -> match x with |((a,b),c) -> if b then a+1 else c+1 |_ -> 0";
      "fun x -> match x with |((a,b),(c,d)) -> if b then a+d else c |_ -> 0";
      "fun x -> match x with |(a,(b,(c,(d,e)))) -> if a then b^c^d^e else \"hello\" |_ -> \"h\"";
      "fun x -> match x with true -> false | false -> true";
      "let rec fact = fun n -> if n=0 then 1 else n * fact (n-1) in fact";
      "match (Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil ())))))) with
       |(Cons(a,Cons(b,Cons(c,Cons(d,Cons(e,Nil())))))) -> a+b+c+d";
      "let rec evil = fun f -> fun g -> fun n ->
       let f' = fun x -> 10 + n in
       if n=1 then f' 0 + f 0 + g 0 else evil f' f (n-1) in
       let dummy = fun x -> 1000 in
       evil dummy dummy 3";
      "match 1729 with x -> x+1";
      "match Cons(1,Nil()) with Nil() -> false | Cons(x,xs) -> true";
      "let rec fib = fun n -> if n < 2 then n else fib (n-1) + fib (n-2) in fib";
      "let rec add = fun n -> fun m -> if n=0 then m else add (n-1) (m+1) in add";
      "let is_empty = fun xs -> match xs with Nil() -> true | Cons(1,t) -> false in is_empty";
      "let has_two = fun xs ->
       match xs with
       | Nil() -> false
       | Cons(x,xs) -> if x then match xs with Nil() -> false | _ -> true else false in
       has_two";
      "let swag = let questionable = (fun x -> match x with Cons(h,Nil()) -> h) in questionable Cons(42,Nil()) in swag"
    ]

  let level4_sol = [
      Parser.parse_type "int * int -> int";
      Parser.parse_type "(int * bool) * int -> int";
      Parser.parse_type "(int * bool) * (int * int) -> int";
      Parser.parse_type "bool * (string * (string * (string * string))) -> string";
      TArrow (TBool, TBool);
      TArrow (TInt, TInt);
      TInt;
      TInt;
      TInt;
      TBool;
      TArrow (TInt, TInt);
      TArrow (TInt, TArrow (TInt, TInt));
      Parser.parse_type "int list -> bool";
      Parser.parse_type "bool list -> bool";
      TInt;
  ]

  let level5_noerr = [
      "fun x -> match x with |(a,b) -> if a then 0 else 1";
      "fun x -> match x with |(((a,b),(c,d)),((e,f),(g,h))) -> if a then b else 1";
      "fun x -> match x with |(a,(b,c)) -> if a then b else \"\"";
      "fun x -> match x with |(((a,b),(c,(d1,(d2,d3)))),((e,f),(g,h))) -> if c then a+b else (if f then g+h else e)";
      "let rec succ_all = fun xs -> match xs with Nil () -> Nil () | Cons(x,xs) -> Cons((x+1),(succ_all xs)) in succ_all";
      "let rec pred_all = fun xs ->
       match xs with Nil () -> Nil () | Cons(1,Nil ()) -> Cons(2,Nil()) | Cons(x,xs) -> Cons((x-1),(pred_all xs)) in pred_all";
      "let zip_with_index = fun xs ->
       let rec zipper = fun xs -> fun n -> match xs with
       | Nil () -> Nil ()
       | Cons(x,xs) -> Cons(Cons(x,Cons(n,Nil())),(zipper xs (n+1))) in
       zipper xs 0 in
       zip_with_index";
      "let rec sum = fun l -> fun seed -> match l with Nil () -> seed | Cons(h,t) -> h + (sum t seed) in let f = sum (Cons(1,Cons(2,Cons(3,Nil())))) in f";
      "let rec nth = fun n -> fun xs -> fun a ->
       if n=0 then match xs with Nil () -> 1+a | Cons(h,_) -> h
       else match xs with Nil() -> 1-a | Cons(_,t) -> nth (n-1) t a in
       nth";
      "let rec and_all = fun xs ->
       match xs with Nil() -> true | Cons(h,t) -> if h then (if and_all t then true else false) else false in
       and_all";
      "let rec filter_by = fun f -> fun xs ->
       match xs with
       Nil () -> Nil ()
       | Cons (h,t) -> if f h then Cons((h+1),(filter_by f t)) else filter_by f t in
       filter_by";
      "let rec permute_thing = fun xs ->
       match xs with
       Cons(1,Cons(2,Cons(3,Nil()))) -> Cons(2,Cons(1,Cons(3,Nil ())))
       | Cons(_,t) -> permute_thing t
       | Nil () -> Nil () in
       permute_thing";
      "let rec go_ham = fun xs -> match xs with
       Cons(1,Cons(x,Nil ())) -> 20+x
       | Cons(h,t) -> h + (go_ham t) in
       go_ham";
      "let rec swag = fun xs ->
       match xs with Cons(x,Cons(y,z)) -> (x (y 1)) + swag z | _ -> 2 in swag";
    ]

  let level5_sol = [
      Parser.parse_type "bool * 'a -> int";
      Parser.parse_type "((bool * int) * ('a * 'b)) * (('c * 'd) * ('e * 'f)) -> int";
      Parser.parse_type "bool * (string * 'a) -> string";
      Parser.parse_type "((int * int) * (bool * ('a * ('b * 'c)))) * ((int * bool) * (int * int)) -> int";
      Parser.parse_type "int list -> int list";
      Parser.parse_type "int list -> int list";
      Parser.parse_type "int list -> int list list";
      Parser.parse_type "int -> int";
      Parser.parse_type "int -> int list -> int -> int";
      Parser.parse_type "bool list -> bool";
      Parser.parse_type "(int -> bool) -> int list -> int list";
      Parser.parse_type "int list -> int list";
      Parser.parse_type "int list -> int";
      Parser.parse_type "(int -> int) list -> int";
    ]

let level6_noerr = [
    "(fun x -> x)";
    "fun x -> fun y -> x";
    "(fun x -> fun y -> fun z -> x z (y z))";
    "let flip = fun f -> fun x -> fun y -> f y x in flip";
    "(fun x -> fun k -> k x)";
    "let bind = fun m -> fun f -> fun k -> m (fun v -> f v k) in bind";
    "let compose = fun f -> fun g -> fun x -> f (g x) in compose";
    "(fun x -> fun y -> fun z -> if x y then z else y z)";
    "(fun x -> fun y -> x y y) (fun x -> fun y -> y)";
    "let a = fun x -> fun y -> fun k -> k (x+y) in
     let s = fun x -> fun k -> k (x*x) in
     let n = fun x -> fun y -> fun k -> s x (fun v -> s y (fun u -> a u v (fun s -> k s))) in
     n";
    "fun n -> fun m -> n < m";
  ]

  let level6_sol =[
    TArrow (TAlpha "a", TAlpha "a");
    TArrow (TAlpha "a", TArrow (TAlpha "b", TAlpha "a"));
    TArrow (TArrow (TAlpha "a", TArrow (TAlpha "b", TAlpha "c")),
           TArrow (TArrow (TAlpha "a", TAlpha "b"),
                  TArrow (TAlpha "a", TAlpha "c")));
    TArrow (TArrow (TAlpha "a", TArrow (TAlpha "b", TAlpha "c")),
           TArrow (TAlpha "b", TArrow (TAlpha "a", TAlpha "c")));
    TArrow (TAlpha "a", TArrow (TArrow (TAlpha "a", TAlpha "b"), TAlpha "b"));
    TArrow (TArrow (TArrow (TAlpha "a", TAlpha "b"), TAlpha "c"),
           TArrow (TArrow (TAlpha "a", TArrow (TAlpha "d", TAlpha "b")),
                  TArrow (TAlpha "d", TAlpha "c")));
    TArrow (TArrow (TAlpha "a", TAlpha "b"),
           TArrow (TArrow (TAlpha "c", TAlpha "a"),
                  TArrow (TAlpha "c", TAlpha "b")));
    TArrow (TArrow (TArrow (TAlpha "a", TAlpha "a"), TBool),
           TArrow (TArrow (TAlpha "a", TAlpha "a"),
                  TArrow (TAlpha "a", TAlpha "a")));
    TArrow (TAlpha "a", TAlpha "a");
    TArrow (TInt, TArrow (TInt, TArrow (TArrow (TInt, TAlpha "a"), TAlpha "a")));
    TArrow (TInt, TArrow (TInt, TBool));
  ]

  let level7_noerr = [
    "let rec fix = fun f -> fun x -> f (fix f) x in fix";
    "let rec ntimes = fun n -> fun f -> fun x -> if n=0 then x else f (ntimes (n-1) f x) in ntimes"
  ]

  let level7_sol = [
    TArrow (TArrow (TArrow (TAlpha "a", TAlpha "b"), TArrow (TAlpha "a", TAlpha "b")),
           TArrow (TAlpha "a", TAlpha "b"));
    TArrow (TInt,
           TArrow (TArrow (TAlpha "a", TAlpha "a"),
                  TArrow (TAlpha "a", TAlpha "a")))
  ]


let level8_noerr = [
  "let rec length = fun xs -> match xs with
   | Nil () -> 0
   | Cons(h,t) -> 1 + length t in length";
  "let rec map = fun f -> fun xs -> match xs with
   | Nil () -> Nil ()
   | Cons(h,t) -> Cons((f h),(map f t)) in map";
  "let rec foldl = fun f -> fun a -> fun xs -> match xs with Nil () -> a | Cons(x,xs) -> foldl f (f a x) xs
   in foldl";
  "let rec filter = fun f -> fun xs -> match xs with
   | Nil () -> Nil ()
   | Cons(x,xs) -> if f x then Cons(x,(filter f xs)) else filter f xs in
   filter";
  "let rev_map = fun f -> fun xs ->
     let rec help = fun xs -> fun a -> match xs with
     | Nil() -> a
     | Cons(x,xs) -> help xs (Cons(((f x),a))) in
     help xs (Nil ()) in
     rev_map";
  "let rec append = fun xs -> fun ys -> match xs with
   | Nil() -> ys
   | Cons(x,t) -> Cons(x,(append t ys)) in
   append";
  "let rec interleave = fun x -> fun xs -> match xs with
   | Nil () -> Nil ()
   | Cons(h,t) -> Cons(x,Cons(h,(interleave x t))) in
   interleave";
  ]

  let level8_sol = [
    Parser.parse_type "'a list -> int";
    Parser.parse_type "('a -> 'b) -> 'a list -> 'b list";
    Parser.parse_type "('a -> 'b -> 'a) -> 'a -> 'b list -> 'a";
    Parser.parse_type "('a -> bool) -> 'a list -> 'a list";
    Parser.parse_type "('a -> 'b) -> 'a list -> 'b list";
    Parser.parse_type "'a list -> 'a list -> 'a list";
    Parser.parse_type "'a -> 'a list -> 'a list";
  ]

  let level9_noerr = [
    "let rec uniq = fun xs -> match xs with
   | Nil () -> Nil ()
   | Cons(x, Nil()) -> Cons(x,Nil ())
   | Cons(h,Cons(ht,tl)) -> if h=ht then uniq (Cons(h,tl)) else Cons(h,(uniq (Cons(ht,tl)))) in
   uniq";
  "let rec by_three = fun f -> fun xs -> match xs with
   | Cons(h,Cons(ht,Cons(htt,t))) -> Cons((f h ht htt),(by_three f t))
   | x -> x in by_three";
  "match Cons(1,Cons(2,Cons(3,Nil()))) with
   | Cons(x,Cons(y,z)) -> (match z with Cons(3,Nil()) -> (y > x) | _ -> false)
   | _ -> false"
  ]

  let level9_sol = [
    Parser.parse_type "int list -> int list";
    Parser.parse_type "('a -> 'a -> 'a -> 'a) -> 'a list -> 'a list";
    TBool
  ]

  let level10_noerr = [
    "let rec fact_cps = fun n -> fun k ->
     if n=0 then k 1 else fact_cps (n-1) (fun x -> k (x*n)) in fact_cps";
    "let dfs = fun t ->
       let rec append = fun x -> fun y -> fun k -> match x with
       | Nil () -> k y
       | Cons(h,t) -> append t y (fun z -> Cons(h,z)) in
       let a = fun x -> fun y -> append x y (fun x -> x) in
     t (fun l -> fun x -> fun r -> Cons(x,(a l r))) (Nil ()) in
     dfs";
    "let rec sort =
       let rec filter = fun f -> fun xs -> match xs with
       | Nil () -> Nil ()
       | Cons(x,xs) -> if f x then Cons(x,(filter f xs)) else filter f xs in
       let rec append = fun x -> fun y -> fun k -> match x with
       | Nil () -> k y
       | Cons(h,t) -> append t y (fun z -> Cons(h,z)) in
       let a = fun x -> fun y -> append x y (fun x -> x) in
       fun l -> match l with
       | Nil()    -> Nil ()
       | Cons(x,xs) ->
         let left = filter (fun n -> n <= x) xs in
         let right = filter (fun n -> n > x) xs in
         a (sort left) (Cons((x,sort right))) in
       sort";
    "(fun x -> x) (fun x -> x)";
  ]

  let level10_sol = [
    Parser.parse_type "int -> (int -> 'a) -> 'a";
    Parser.parse_type "(('a list -> 'a -> 'a list -> 'a list) -> 'b list -> 'c) -> 'c";
    Parser.parse_type "int list -> int list";
    Parser.parse_type "'a -> 'a";
  ]

  (*complex variants*)
  (*look at top of file to see what specific cases refer to
   and what they are testing *)
  let level11_noerr = [rt1;rt2;rt3;rt4;rt5;rt6;rt7;rt8;rt9;rt10;rt11;rt12;option_test]

  let level11_sol = [rt_sol_1;rt_sol_2;rt_sol_3;rt_sol_4;rt_sol_5;rt_sol_6;
                                        rt_sol_7;rt_sol_8;rt_sol_9;rt_sol_10;rt_sol_11;rt_sol_12;option_test_sol]

  (*these should be errors*)
  let level11_err = [err_rt1;err_rt2;err_rt3;err_rt4;err_rt5;err_rt6]

  (*match cases for complex variants*)
  (*includes non polymorphic and polymorphic*)
  let level12_noerr = [rt_match_1;rt_match_2;rt_match_3;rt_match_4;rt_match_5;rt_match_6;rt_match_7;rt_fold]
  let level12_sol = [rt_match_1_sol;rt_match_2_sol;rt_match_3_sol;rt_match_4_sol;
                                                rt_match_5_sol;rt_match_6_sol;rt_match_7_sol;rt_fold_sol]

  (*nested variants/multiple variants used together*)
  (*look at top of file to see what specific cases refer to
    and what they are testing*)
  let level13_noerr = [rem;push;transfer;pop]

  let level13_sol = [rem_sol;push_sol;transfer_sol;pop_sol]


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
  level11_noerr;
  level12_noerr;
  level13_noerr
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
  level11_sol;
  level12_sol;
  level13_sol
]

let timed f = Assertions.timeout 2 f ()

let spec_list = [option_spec;list_spec;rt_spec;queue_spec]

exception Test_failed of string
let run_test i j =
  let test_str = List.nth  (List.nth levels (i-1)) j in
  let test = Parser.parse_expr test_str in
  let sol_type = List.nth (List.nth  solutions (i-1)) j in
  let student_type = typeof (infer spec_list test) in
  match student_type = sol_type with
  |true -> ()
  |false -> begin
      raise (Test_failed (Format.sprintf "Test: %s" test_str))
    end

TEST_UNIT "annotate1-1" = run_test 1 0
TEST_UNIT "annotate1-2" = run_test 1 1
TEST_UNIT "annotate1-3" = run_test 1 2
TEST_UNIT "annotate1-4" = run_test 1 3
TEST_UNIT "annotate1-5" = run_test 1 4
TEST_UNIT "annotate1-6" = run_test 1 5
TEST_UNIT "annotate1-7" = run_test 1 6
TEST_UNIT "annotate1-8" = run_test 1 7
TEST_UNIT "annotate1-9" = run_test 1 8
TEST_UNIT "annotate1-10" = run_test 1 9
TEST_UNIT "annotate1-11" = run_test 1 10
TEST_UNIT "annotate1-12" = run_test 1 11
TEST_UNIT "annotate1-13" = run_test 1 12
TEST_UNIT "annotate1-14" = run_test 1 13
TEST_UNIT "annotate2-1" = run_test 2 0
TEST_UNIT "annotate2-2" = run_test 2 1
TEST_UNIT "annotate2-3" = run_test 2 2
TEST_UNIT "annotate2-4" = run_test 2 3
TEST_UNIT "annotate2-5" = run_test 2 4
TEST_UNIT "annotate2-6" = run_test 2 5
TEST_UNIT "annotate2-7" = run_test 2 6
TEST_UNIT "annotate2-8" = run_test 2 7
TEST_UNIT "annotate2-9" = run_test 2 8
TEST_UNIT "annotate2-10" = run_test 2 9
TEST_UNIT "annotate2-11" = run_test 2 10
TEST_UNIT "annotate2-12" = run_test 2 11
TEST_UNIT "annotate3-1" = run_test 3 0
TEST_UNIT "annotate3-2" = run_test 3 1
TEST_UNIT "annotate3-3" = run_test 3 2
TEST_UNIT "annotate3-4" = run_test 3 3
TEST_UNIT "annotate3-5" = run_test 3 4
TEST_UNIT "annotate3-6" = run_test 3 5
TEST_UNIT "annotate3-7" = run_test 3 6
TEST_UNIT "annotate3-8" = run_test 3 7
TEST_UNIT "annotate3-9" = run_test 3 8
TEST_UNIT "annotate4-1" = run_test 4 0
TEST_UNIT "annotate4-2" = run_test 4 1
TEST_UNIT "annotate4-3" = run_test 4 2
TEST_UNIT "annotate4-4" = run_test 4 3
TEST_UNIT "annotate4-5" = run_test 4 4
TEST_UNIT "annotate4-6" = run_test 4 5
TEST_UNIT "annotate4-7" = run_test 4 6
TEST_UNIT "annotate4-8" = run_test 4 7
TEST_UNIT "annotate4-9" = run_test 4 8
TEST_UNIT "annotate4-10" = run_test 4 9
TEST_UNIT "annotate4-11" = run_test 4 10
TEST_UNIT "annotate4-12" = run_test 4 11
TEST_UNIT "annotate4-13" = run_test 4 12
TEST_UNIT "annotate4-14" = run_test 4 13
TEST_UNIT "annotate4-15" = run_test 4 14
TEST_UNIT "annotate5-1" = run_test 5 0
TEST_UNIT "annotate5-2" = run_test 5 1
TEST_UNIT "annotate5-3" = run_test 5 2
TEST_UNIT "annotate5-4" = run_test 5 3
TEST_UNIT "annotate5-5" = run_test 5 4
TEST_UNIT "annotate5-6" = run_test 5 5
TEST_UNIT "annotate5-7" = run_test 5 6
TEST_UNIT "annotate5-8" = run_test 5 7
TEST_UNIT "annotate5-10" = run_test 5 9
TEST_UNIT "annotate5-11" = run_test 5 10
TEST_UNIT "annotate5-12" = run_test 5 11
TEST_UNIT "annotate5-13" = run_test 5 12
TEST_UNIT "annotate5-14" = run_test 5 13
TEST_UNIT "annotate6-1" = run_test 6 0
TEST_UNIT "annotate6-2" = run_test 6 1
TEST_UNIT "annotate6-3" = run_test 6 2
TEST_UNIT "annotate6-4" = run_test 6 3
TEST_UNIT "annotate6-5" = run_test 6 4
TEST_UNIT "annotate6-6" = run_test 6 5
TEST_UNIT "annotate6-7" = run_test 6 6
TEST_UNIT "annotate6-8" = run_test 6 7
TEST_UNIT "annotate6-9" = run_test 6 8
TEST_UNIT "annotate6-10" = run_test 6 9
TEST_UNIT "annotate6-11" = run_test 6 10
TEST_UNIT "annotate7-1" = run_test 7 0
TEST_UNIT "annotate7-2" = run_test 7 1
TEST_UNIT "annotate8-1" = run_test 8 0
TEST_UNIT "annotate8-2" = run_test 8 1
TEST_UNIT "annotate8-3" = run_test 8 2
TEST_UNIT "annotate8-4" = run_test 8 3
TEST_UNIT "annotate8-5" = run_test 8 4
TEST_UNIT "annotate8-6" = run_test 8 5
TEST_UNIT "annotate8-7" = run_test 8 6
TEST_UNIT "annotate9-1" = run_test 9 0
TEST_UNIT "annotate9-2" = run_test 9 1
TEST_UNIT "annotate9-3" = run_test 9 2
TEST_UNIT "annotate10-1" = run_test 10 0
TEST_UNIT "annotate10-2" = run_test 10 1
TEST_UNIT "annotate10-3" = run_test 10 2
TEST_UNIT "annotate10-4" = run_test 10 3
TEST_UNIT "annotate11-1" = run_test 11 0
TEST_UNIT "annotate11-2" = run_test 11 1
TEST_UNIT "annotate11-3" = run_test 11 2
TEST_UNIT "annotate11-4" = run_test 11 3
TEST_UNIT "annotate11-5" = run_test 11 4
TEST_UNIT "annotate11-6" = run_test 11 5
TEST_UNIT "annotate11-7" = run_test 11 6
TEST_UNIT "annotate11-8" = run_test 11 7
TEST_UNIT "annotate11-9" = run_test 11 8
TEST_UNIT "annotate11-10" = run_test 11 9
TEST_UNIT "annotate11-11" = run_test 11 10
TEST_UNIT "annotate11-12" = run_test 11 11
TEST_UNIT "annotate11-13" = run_test 11 12
TEST_UNIT "annotate12-1" = run_test 12 0
TEST_UNIT "annotate12-2" = run_test 12 1
TEST_UNIT "annotate12-3" = run_test 12 2
TEST_UNIT "annotate12-4" = run_test 12 3
TEST_UNIT "annotate12-5" = run_test 12 4
TEST_UNIT "annotate12-6" = run_test 12 5
TEST_UNIT "annotate12-7" = run_test 12 6
TEST_UNIT "annotate12-8" = run_test 12 7
TEST_UNIT "annotate13-1" = run_test 13 0
TEST_UNIT "annotate13-2" = run_test 13 1
TEST_UNIT "annotate13-3" = run_test 13 2
TEST_UNIT "annotate13-4" = run_test 13 3
