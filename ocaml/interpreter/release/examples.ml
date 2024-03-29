open Streams
open Ast
open TypedAst
open Infer
open Eval

(******************************************************************************)
(* examples from part 2 *******************************************************)
(******************************************************************************)

let rec sevens = 7::sevens

let rec sevens_str = Stream(7, fun () -> sevens_str)

let%test _ = take 8 sevens_str = [7; 7; 7; 7; 7; 7; 7; 7]

let rec eights_str = Stream(8, fun () -> eights_str)

let%test _ = take 8 eights_str = [8; 8; 8; 8; 8; 8; 8; 8]

let lazy_v = fun () -> failwith "yolo"

let%test _ = take 8 (fibs ()) = [0; 1; 1; 2; 3; 5; 8; 13]

let%test _ = take 2 (pi ())  = [4.; 4. -. 4. /. 3.]

let%test _ = take 5 (look_and_say ()) = [[1]; [1;1]; [2;1]; [1;2;1;1]; [1;1;1;2;2;1]]

(******************************************************************************)
(* examples from part 3 *******************************************************)
(******************************************************************************)

let expr_example  = Parser.parse_expr "fun x -> 3 + x"
let expr_example' = Fun ("x", BinOp (Plus, Int 3, Var "x"))

let%test _ = expr_example = expr_example'

let eval_example  = Parser.parse_expr "if false then 3 + 5 else 3 * 5"
let eval_example' = If (Bool false, BinOp (Plus, Int 3, Int 5), BinOp (Times, Int 3, Int 5))

let%test _ = eval_example = eval_example'
let%test _ = eval [] eval_example = VInt 15

(******************************************************************************)
(* examples from part 4 *******************************************************)
(******************************************************************************)

let infer_example  = Parser.parse_expr "fun x -> 3 + x"
let infer_example' = Fun ("x", BinOp(Plus, Int 3, Var "x"))

let%test _ = infer_example = infer_example'

let infer_example_type  = Parser.parse_type "int -> int"
let infer_example_type' = TArrow (TInt,TInt)

let%test _ = infer_example_type = infer_example_type'

let%test _ = typeof (infer [] infer_example) = infer_example_type


let option_spec  = Parser.parse_variant_spec "type 'a option = Some of 'a | None of unit"
let option_spec' = {
  vars = ["a"];
  name = "option";
  constructors = [
    "Some", TAlpha "a";
    "None", TUnit;
  ];
}

let%test _ = option_spec = option_spec'


let list_spec  = Parser.parse_variant_spec "type 'a list = Nil of unit | Cons of ('a * 'a list)"
let list_spec' = {
  vars = ["a"];
  name = "list";
  constructors = [
    "Nil",  TUnit;
    "Cons", TStar (TAlpha "a", TVariant ([TAlpha "a"], "list"));
  ];
}

let%test _ = list_spec = list_spec'


let infer_variant  = Parser.parse_expr "(Some 1, Some \"where\")"
let infer_variant' = Pair (Variant ("Some", Int 1),
                           Variant ("Some", String "where"))

let%test _ = infer_variant = infer_variant'


let infer_variant_type  = Parser.parse_type "int option * string option"
let infer_variant_type' = TStar (TVariant ([TInt], "option"), TVariant ([TString], "option"))

let%test _ = infer_variant_type = infer_variant_type'

let%test _ = typeof (infer [option_spec] infer_variant) = infer_variant_type



(** Note: the infer_poly example will fail unless you implement the Karma
    problem. *)

let infer_poly  = Parser.parse_expr "let any = fun x -> x in (any 1, any \"where\")"
let infer_poly' = Let ("any", Fun ("x", Var "x"),
                      Pair (App (Var "any", Int 1),
                            App (Var "any", String "where")))

let%test _ = infer_poly = infer_poly'

let infer_poly_type  = Parser.parse_type "int * string"
let infer_poly_type' = TStar (TInt, TString)

let%test _ = infer_poly_type = infer_poly_type'

let%test _ = typeof (infer [] infer_poly) = infer_poly_type

(******************************************************************************)
(** other examples ************************************************************)
(******************************************************************************)

let map  = Parser.parse_expr "let rec map = fun f -> fun l -> match l with
                                | Nil ()       -> Nil ()
                                | Cons (hd,tl) -> Cons (f hd, map f tl)
                              in map"
let map' = LetRec ("map", Fun ("f", Fun ("l",
                            Match (Var "l", [
                              PVariant ("Nil",  PUnit),
                                Variant ("Nil", Unit);
                              PVariant ("Cons", PPair(PVar "hd", PVar "tl")),
                                Variant ("Cons", Pair (App (Var "f", Var "hd")
                                                      ,App (App (Var "map", Var "f"), Var "tl")
                                                      ));
                            ]))),
                   Var "map")

let%test _ = map = map'

let map_type  = Parser.parse_type "('a -> 'b) -> 'a list -> 'b list"
let map_type' = TArrow(TArrow(TAlpha "a", TAlpha "b"),
                       TArrow(TVariant ([TAlpha "a"], "list"),
                              TVariant ([TAlpha "b"], "list")))


let%test _ = map_type = map_type'

let%test _ = typeof (infer [list_spec] map) = map_type

let fold  = Parser.parse_expr "let rec fold = fun f -> fun l -> fun a -> match l with
                                 | Nil () -> a
                                 | Cons (hd,tl) -> f hd (fold f tl a)
                               in fold"
let fold' = LetRec ("fold", Fun ("f", Fun ("l", Fun ("a", Match (Var "l", [
              PVariant ("Nil", PUnit),
                Var "a";
              PVariant ("Cons", PPair (PVar "hd", PVar "tl")),
                App (App (Var "f", Var "hd"), App (App (App (Var "fold", Var "f"), Var "tl"), Var "a"))
           ])))), Var "fold")

let%test _ = fold = fold'

let fold_type = Parser.parse_type "('a -> 'b -> 'b) -> 'a list -> 'b -> 'b"

let%test _ = typeof (infer [list_spec] fold) = fold_type
