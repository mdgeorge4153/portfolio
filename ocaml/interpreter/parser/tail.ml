# 1 tail.ml


let parse_expr s =
  expr token (Lexing.from_string s)

let parse_type s =
  typ  token (Lexing.from_string s)

let parse_variant_spec s =
  variant_spec token (Lexing.from_string s)

type filename = string

let parse_expr_from_file f =
  expr token (Lexing.from_channel (open_in f))

let parse_type_from_file f =
  typ token (Lexing.from_channel (open_in f))

let parse_variant_spec_from_file f =
  variant_spec token (Lexing.from_channel (open_in f))

(** Regression tests *)

TEST =
  parse_type "('a,'b) choice"
    = TVariant([TAlpha "a"; TAlpha "b"], "choice");

TEST = 
  parse_variant_spec "type ('a, 'b) mystery = Alpha of 'a | Beta of 'b | Gamma of ('a * ('a, 'b) mystery)"
    =
  {
    vars = ["a"; "b"]; name="mystery";
    constructors = [
      "Alpha", TAlpha "a";
      "Beta",  TAlpha "b";
      "Gamma", TStar (TAlpha "a", TVariant([TAlpha "a"; TAlpha "b"], "mystery"))
    ];
  }

TEST =
  parse_expr "if true then (true, \"hello\") else (false, \"hi\")"
    =
  If (Bool true, Pair (Bool true, String "hello"),
                 Pair (Bool false, String "hi"))


