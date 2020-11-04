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

  let level1_sol : Ast.typ list = [
    TInt;
    TBool;
    TInt;
    TBool;
    TBool;
    TBool;
    TInt;
    TBool;
    TList (TList TInt);
    TBool;
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

  let level2_sol : Ast.typ list = [
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
  ]

(** Higher order functions, no polymorphism *)
  let level3_noerr = [
    "fun n -> n+1";
    "(fun b c -> if b = c then true else false)";
    "let x = 7 in fun y -> x+y";
    "let x = 7 in (fun x -> x+x) 17";
    "fun x y -> (x-1)::y";
    "(fun x y -> let x = 7 in x + (x * y)) true";
    "(fun x y z -> x (y+z) + 1)";
    "let f x = x - (1 * x) in (fun f -> f 42) f";
    "fun g h x -> g ((h (x+1)) + 1) + 1";
    "(fun f g x -> f (g x) + g (f x))";
  ]

  let level3_sol = [
    Arrow (TInt,TInt);
    Arrow (TBool, Arrow (TBool, TBool));
    Arrow (TInt, TInt);
    TInt;
    Arrow (TInt, Arrow (TList TInt, TList TInt));
    Arrow (TInt, TInt);
    Arrow (Arrow (TInt, TInt), Arrow (TInt, Arrow (TInt, TInt)));
    TInt;
    Arrow (Arrow (TInt, TInt), Arrow (Arrow (TInt, TInt), Arrow (TInt, TInt)));
    Arrow (Arrow (TInt, TInt), Arrow (Arrow (TInt, TInt), Arrow (TInt, TInt)));
  ]

(** Matching and recursion, no polymorphism *)
  let level4_noerr = [
    "fun x -> match x with true -> false | false -> true";
    "let rec fact n = if n=0 then 1 else n * fact (n-1) in fact";
    "match [1;2;3;4;5] with a::b::c::d::e -> a + b + c + d";
    "let rec evil f g n =
     let f' x = 10 + n in
     if n=1 then f' 0 + f 0 + g 0 else evil f' f (n-1) in
     let dummy x = 1000 in
     evil dummy dummy 3";
    "match 1729 with x -> x+1";
    "match [1] with [] -> false | x::xs -> true";
    "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in fib";
    "let rec add n m = if n=0 then m else add (n-1) (m+1) in add";
    "let is_empty xs = match xs with [] -> true | 1::t -> false in is_empty";
    "let has_two xs =
     match xs with
     | [] -> false
     | x::xs -> if x || (not x) then match xs with [] -> false | _ -> true else false in
     has_two";
    "let swag = let questionable = (fun x -> match x with [h] -> h) in questionable [42] in swag"
  ]

  let level4_sol = [
    Arrow (TBool, TBool);
    Arrow (TInt, TInt);
    TInt;
    TInt;
    TInt;
    TBool;
    Arrow (TInt, TInt);
    Arrow (TInt, Arrow (TInt, TInt));
    Arrow (TList TInt, TBool);
    Arrow (TList TBool, TBool);
    TInt;
  ]

  (** List processing with no polymorphism *)
  let level5_noerr = [
    "let rec succ_all xs = match xs with [] -> [] | x::xs -> (x+1)::(succ_all xs) in succ_all";
    "let rec pred_all xs =
     match xs with [] -> [] | [1] -> [2] | x::xs -> (x-1)::pred_all xs in pred_all";
    "let zip_with_index xs =
     let rec zipper xs n = match xs with
     | [] -> []
     | x::xs -> [x;n]::(zipper xs (n+1)) in
     zipper xs 0 in
     zip_with_index";
    "let rec sum l seed = match l with [] -> seed | h::t -> h + (sum t seed) in let f = sum [1;2;3] in f";
    "let rec nth n xs a =
       if n=0 then match xs with [] -> 1+a | h::_ -> h
       else match xs with [] -> 1-a | _::t -> nth (n-1) t a in
       nth";
    "let rec and_all xs =
       match xs with [] -> true | h::t -> h && and_all t in
       and_all";
    "let rec filter_by f xs =
       match xs with
        [] -> []
      | h::t -> if f h then (h+1)::(filter_by f t) else filter_by f t in
      filter_by";
    "let rec permute_thing xs =
       match xs with
         [1;2;3] -> [2;1;3]
       | _::t -> permute_thing t
       | [] -> [] in
     permute_thing";
    "let rec go_ham xs = match xs with
       [1;x] -> 20+x
     | h::t -> h+(go_ham t) in
     go_ham";
    "let rec swag xs =
       match xs with x::y::z -> x (y 1) + swag z | _ -> 2 in swag";
  ]

  let level5_sol = [
    Arrow (TList TInt, TList TInt);
    Arrow (TList TInt, TList TInt);
    Arrow (TList TInt, TList (TList TInt));
    Arrow (TInt, TInt);
    Arrow (TInt, Arrow (TList TInt, Arrow (TInt, TInt)));
    Arrow (TList TBool, TBool);
    Arrow (Arrow (TInt, TBool), Arrow (TList TInt, TList TInt));
    Arrow (TList TInt, TList TInt);
    Arrow (TList TInt, TInt);
    Arrow (TList (Arrow (TInt, TInt)), TInt);
  ]

  (** (Non-recursive) Functions with polymorphism *)
  let level6_noerr = [
    "(fun x -> x)";
    "fun x y -> x";
    "(fun x y z -> x z (y z))";
    "let flip f x y = f y x in flip";
    "(fun x k -> k x)";
    "let bind m f = fun k -> m (fun v -> f v k) in bind";
    "let compose f g x = f (g x) in compose";
    "(fun x y z -> if x y then z else y z)";
    "(fun x y -> x y y) (fun x y -> y)";
    "let a x y k = k (x+y) in
     let s x k = k (x*x) in
     let n x y k = s x (fun v -> s y (fun u -> a u v (fun s -> k s))) in
     n";
    "fun n m -> n < m";
  ]

  let level6_sol : Ast.typ list =[
    Arrow (TVar "a", TVar "a");
    Arrow (TVar "a", Arrow (TVar "b", TVar "a"));
    Arrow (Arrow (TVar "a", Arrow (TVar "b", TVar "c")),
           Arrow (Arrow (TVar "a", TVar "b"),
                  Arrow (TVar "a", TVar "c")));
    Arrow (Arrow (TVar "a", Arrow (TVar "b", TVar "c")),
           Arrow (TVar "b", Arrow (TVar "a", TVar "c")));
    Arrow (TVar "a", Arrow (Arrow (TVar "a", TVar "b"), TVar "b"));
    Arrow (Arrow (Arrow (TVar "a", TVar "b"), TVar "c"),
           Arrow (Arrow (TVar "a", Arrow (TVar "d", TVar "b")),
                  Arrow (TVar "d", TVar "c")));
    Arrow (Arrow (TVar "b", TVar "c"),
           Arrow (Arrow (TVar "a", TVar "b"),
                  Arrow (TVar "a", TVar "c")));
    Arrow (Arrow (Arrow (TVar "a", TVar "a"), TBool),
           Arrow (Arrow (TVar "a", TVar "a"),
                  Arrow (TVar "a", TVar "a")));
    Arrow (TVar "a", TVar "a");
    Arrow (TInt, Arrow (TInt, Arrow (Arrow (TInt, TVar "a"), TVar "a")));
    Arrow (TVar "a", Arrow (TVar "a", TBool));
  ]

(** Recursive functions with polymorphism *)
  let level7_noerr = [
    "let rec fix f x = f (fix f) x in fix";
    "let rec ntimes n f x = if n=0 then x else f (ntimes (n-1) f x) in ntimes";
    "let rec alt n f g x =
     if n=0 then x else
     if n mod 2 = 0 then f (alt (n-1) f g x) else g (alt (n-1) f g x) in
     alt"
  ]

  let level7_sol : Ast.typ list = [
    Arrow (Arrow (Arrow (TVar "a", TVar "b"), Arrow (TVar "a", TVar "b")),
           Arrow (TVar "a", TVar "b"));
    Arrow (TInt,
           Arrow (Arrow (TVar "a", TVar "a"),
                  Arrow (TVar "a", TVar "a")));
    Arrow (TInt,
           Arrow (Arrow (TVar "a", TVar "a"),
                  Arrow (Arrow (TVar "a", TVar "a"),
                         Arrow (TVar "a", TVar "a"))));
  ]

(** Lists with polymorphism *)
  let level8_noerr = [
  "let rec length xs = match xs with
   | [] -> 0
   | h::t -> 1 + length t in length";
  "let rec map f xs = match xs with
   | [] -> []
   | h::t -> (f h) :: (map f t) in map";
  "let rec foldl f a xs = match xs with [] -> a | x::xs -> foldl f (f a x) xs
   in foldl";
  "let rec filter f xs = match xs with
   | [] -> []
   | x::xs -> if f x then x::(filter f xs) else filter f xs in
   filter";
  "let rev_map f xs =
     let rec help xs a = match xs with
     | [] -> a
     | x::xs -> help xs ((f x)::a) in
     help xs [] in
     rev_map";
  "let rec append xs ys = match xs with
   | [] -> ys
   | x::t -> x::(append t ys) in
   append";
  "let rec interleave x xs = match xs with
   | [] -> []
   | h::t -> x::h::(interleave x t) in
   interleave";
  ]

  let level8_sol : Ast.typ list = [
    Arrow (TList (TVar "a"), TInt);
    Arrow (Arrow (TVar "a", TVar "b"),
           Arrow (TList (TVar "a"), TList (TVar "b")));
    Arrow (Arrow (TVar "a", Arrow (TVar "b", TVar "a")),
           Arrow (TVar "a", Arrow (TList (TVar "b"), TVar "a")));
    Arrow (Arrow (TVar "a", TBool),
           Arrow (TList (TVar "a"), TList (TVar "a")));
    Arrow (Arrow (TVar "a", TVar "b"),
           Arrow (TList (TVar "a"), TList (TVar "b")));
    Arrow (TList (TVar "a"), Arrow (TList (TVar "a"), TList (TVar "a")));
    Arrow (TVar "a", Arrow (TList (TVar "a"), TList (TVar "a")));
  ]

(** Complex Match Cases*)
  let level9_noerr = [
    "let rec uniq xs = match xs with
   | [] -> []
   | [x] -> [x]
   | h::ht::tl -> if h=ht then uniq (h::tl) else h::(uniq (ht::tl)) in
   uniq";
  "let rec by_three f xs = match xs with
   | h::ht::htt::t -> (f h ht htt)::(by_three f t)
   | x -> x in by_three";
  "match [1;2;3] with
   | x::y::z -> (match z with [3] -> (y > x) && true | _ -> false)
   | _ -> false"
  ]

  let level9_sol : Ast.typ list = [
    Arrow (TList (TVar "a"), TList (TVar "a"));
    Arrow (Arrow (TVar "a", Arrow (TVar "a", (Arrow (TVar "a", TVar "a")))),
           Arrow (TList (TVar "a"), TList (TVar "a")));
    TBool
  ]

(** Hard Test Cases *)
  let level10_noerr = [
    "let rec fact_cps n k =
     if n=0 then k 1 else fact_cps (n-1) (fun x -> k (x*n)) in fact_cps";
    "let dfs t =
       let rec append x y k = match x with
       | [] -> k y
       | h::t -> append t y (fun z -> h::z) in
       let a x y = append x y (fun x -> x) in
     t (fun l x r -> x::(a l r)) [] in
     dfs";
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
       sort";
    "(fun x -> x) (fun x -> x)";
  ]

  let level10_sol : Ast.typ list = [
    Arrow (TInt, Arrow (Arrow (TInt, TVar "a"), TVar "a"));
    Arrow (
      Arrow (
        Arrow (
          TList (TVar "a"),
          Arrow (TVar "a",
                 Arrow (TList (TVar "a"), TList (TVar "a")))),
        Arrow (TList (TVar "b"), TVar "c")), TVar "c");
    Arrow (TList (TVar "a"), TList (TVar "a"));
    Arrow (TVar "a", TVar "a");
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

module InferSol = struct
  open Ast
  open Exceptions
  open Printer

  let char_count  = ref (Char.code 'a')
  let alpha_count = ref 0

(* start back at 'a *)
  let reset_type_vars () =
    char_count  := Char.code 'a';
    alpha_count := 0

  let rec next_type_var () : typ =
    let helper () =
      let result =
        ((String.make 1 (Char.chr (!char_count)))^
            (if !alpha_count = 0
             then ""
             else string_of_int (!alpha_count))) in
      if !char_count >= Char.code 'z' then begin
        char_count := Char.code 'a';
        incr alpha_count end
      else incr char_count;
      result in
    TVar (helper ())

  let type_of = function
    | ABool       (_, a)
    | AInt        (_, a)
    | ANil        a
    | AUnit       a
    | ACons       (_, _, a)
    | AIfThenElse (_, _, _, a)
    | ALetRec     (_, _, _, _, a)
    | ALet        (_, _, _, _, a)
    | ABinaryOp   (_,_,_,a)
    | AUnaryOp    (_,_,a)
    | AVar        (_, a)
    | AFun        (_, _, a)
    | AApp        (_, _, a)
    | AMatch      (_, _, a) -> a

  let type_of_pattern = function
    | APConstant (_, t)
    | APVar      (_,t)
    | APCons     (_, _, t) -> t

  module VarSet = Set.Make (struct
    type t = id
    let compare = Pervasives.compare
  end)

  let alpha_vary (t : typ) : typ =
    let open VarSet in
    let rec collect_tvars tvars = function
      | TBool
      | TUnit
      | TInt          -> tvars
      | TList t       -> collect_tvars tvars t
      | TVar  id      -> add id tvars
      | Arrow (t1,t2) -> begin
        union (collect_tvars tvars t1) (collect_tvars tvars t2)
      end in
    let rec fresh_vars n acc =
      if n = 0 then List.rev acc else fresh_vars (n-1) (next_type_var()::acc) in
    let ids = collect_tvars VarSet.empty t in
    let s   =
      List.combine (VarSet.elements ids) (fresh_vars (VarSet.cardinal ids) []) in
    let rec replace s = function
      | TVar x      -> List.assoc x s
      | TBool       -> TBool
      | TInt        -> TInt
      | TUnit       -> TUnit
      | TList u     -> TList (replace s u)
      | Arrow (u,v) -> Arrow (replace s u, replace s v) in
    replace s t

  let rec annotate (e : expr) (fvs : (id, typ) Hashtbl.t) : aexpr =
    annotate' fvs e []

  and annotate' (fvs : (id,typ) Hashtbl.t) (e : expr) (bv : (id * typ) list) : aexpr =
    match e with
    | Constant c -> begin match c with
      | Bool b -> ABool (b, TBool)
      | Int x  -> AInt (x, TInt)
      | Unit   -> AUnit (TUnit)
      | Nil    -> ANil (TList (next_type_var()))
    end
    | Var x -> begin
      try
      (* bound variable? *)
        let a = List.assoc x bv in AVar (x, a)
      with Not_found -> begin
      (* known free variable? *)
        try let a = Hashtbl.find fvs x in AVar (x, alpha_vary a)
      (* unknown free variable *)
        with Not_found -> unbound_var x
      end
    end
    | Cons (e1, e2) -> begin
      let ae1 = annotate' fvs e1 bv in
      ACons (ae1, annotate' fvs e2 bv, TList (type_of ae1))
    end
    | IfThenElse (e1, e2, e3) -> begin
      let ae1 = annotate' fvs e1 bv in
      let ae2 = annotate' fvs e2 bv in
      let ae3 = annotate' fvs e3 bv in
      AIfThenElse (ae1, ae2, ae3, type_of ae2)
    end
    | LetRec (x, e1, e2) -> begin
      let a = next_type_var () in
      let ae1 = annotate' fvs e1 ((x, a) :: bv) in
      let ae2 = annotate' fvs e2 ((x, a) :: bv) in
      ALetRec (x, a, ae1, ae2, type_of ae2)
    end
    | Let (x, e1, e2) -> begin
      let a   = next_type_var() in
      let ae1 = annotate' fvs e1 bv in
      let ae2 = match type_of ae1 with
        | _      -> annotate' fvs e2 ((x,a)::bv) in
      ALet (x, a, ae1, ae2, type_of ae2)
    end
    | BinaryOp _
    | UnaryOp  _ as e' -> annotate_op fvs bv e'
    | Fun (x, e) ->
    (* assign a new type to x *)
      let a = next_type_var() in
      let ae = annotate' fvs e ((x, a) :: bv) in
      AFun (x, ae, Arrow (a, type_of ae))
    | App (e1, e2) -> begin
      AApp (annotate' fvs e1 bv, annotate' fvs e2 bv, next_type_var())
    end
    | Match (e1, ps) -> begin
      let rec annotate_p p = match p with
        | PConstant c -> begin match c with
          | Bool b -> APConstant (c, TBool)
          | Int i -> APConstant (c, TInt)
          | Unit -> APConstant (c,TUnit)
          | Nil -> APConstant (c, TList (next_type_var()))
        end
        | PVar x -> APVar(x, next_type_var())
        | PCons (p1, p2) ->
          let ap1 = annotate_p p1 in
          APCons (ap1, annotate_p p2, TList (type_of_pattern ap1)) in
      let disjoint l =
        let rec helper = function
          | (x1,t1)::(x2,t2)::xs -> if x1 = x2 then false else helper ((x2,t2)::xs)
          | _ -> true in
        helper (List.sort compare l) in
      let rec bound_vars = function
        | APConstant _ -> []
        | APVar (x,t) -> [(x,t)]
        | APCons (ap1, ap2, _) -> begin
          List.rev_append (bound_vars ap1) (bound_vars ap2)
        end in
      let helper (p, e2) =
        let ap = annotate_p p in
        let bvs = bound_vars ap in
        if disjoint bvs then
          (ap, annotate' fvs e2 (List.rev_append bvs bv))
        else runtime_error "Bound duplicate variables in match" in
      let aps_and_aes = List.map helper ps in
      let t = match aps_and_aes with
        | [] -> parse_error "Parsing match case resulted in invalid AST."
        | (_, ae1)::_ -> type_of ae1 in
      AMatch (annotate' fvs e1 bv, aps_and_aes, t)
    end

  and annotate_op fvs bv = function
    | BinaryOp (op,l,r) -> begin
      match op with
      | Plus
      | Minus
      | Mult
      | Divide
      | Mod    -> ABinaryOp (op,annotate' fvs l bv, annotate' fvs r bv, TInt)
      | Gt
      | Lt
      | Ltq
      | Gtq
      | Eq
      | Neq
      | And
      | Or     -> ABinaryOp (op,annotate' fvs l bv,annotate' fvs r bv, TBool)
    end
    | UnaryOp (op,e)  -> begin
      match op with
      | Not   ->  AUnaryOp (op,annotate' fvs e bv, TBool)
    end
    | _ as e -> begin
      let msg = Printf.sprintf
        "The expression: %s\n is not an operator, but an operator was expected."
        (expr_to_string e) in
      runtime_error msg
    end

  let add_constraint  t  t' u = (t,t')::u
  let add_constraints cs u    = List.fold_left (fun a c -> c::a) u cs

(* collect constraints for unification *)
  let rec collect aexprs u = match aexprs with
    | [] -> u
    | ae::aes -> begin
      match ae with
      | ABool _
      | AInt  _
      | AVar  _
      | AUnit _
      | ANil  _ -> collect aes u
      | ACons (ae1, ae2, _) -> begin
        let t1, t2 = type_of ae1, type_of ae2 in
        collect (ae1 :: ae2 :: aes) ((TList t1, t2) :: u)
      end
      | ABinaryOp _
      | AUnaryOp  _ as e -> collect_operator_contraints aes u e
      | AIfThenElse (ae1, ae2, ae3, a) -> begin
        let (t1,t2,t3) = (type_of ae1, type_of ae2, type_of ae3) in
        collect (ae1 :: ae2 :: ae3 :: aes) ((t1, TBool) :: (t2, t3) :: u)
      end
      | ALetRec (x, tx, ae1, ae2, a)
      | ALet    (x, tx, ae1, ae2, a) -> begin
        let (t1, t2) = (type_of ae1, type_of ae2) in
        collect (ae1 :: ae2 :: aes) ((tx, t1) :: u)
      end
      | AFun (_, ae, _) -> collect (ae :: aes) u
      | AApp (ae1, ae2, a) -> begin
        let (f, b) = (type_of ae1, type_of ae2) in
        collect (ae1 :: ae2 :: aes) ((f, Arrow (b, a)) :: u)
      end
      | AMatch (ae1, aps_and_aes, a) -> begin
        let t1 = type_of ae1 in
        let rec collect_ap = function
          | APConstant _ -> []
          | APVar _ -> []
          | APCons (ap1, ap2, a) ->
            let (t1, t2) = (type_of_pattern ap1, type_of_pattern ap2) in
            List.rev_append ((TList t1, t2) :: collect_ap ap1) (collect_ap ap2) in
        let collect_p acc (ap', ae') =
          List.rev_append (collect_ap ap')
            ((type_of_pattern ap', t1)::(type_of ae', a)::acc) in
        let cs = List.fold_left collect_p [] aps_and_aes in
        let aes = List.map (fun (_,ae) -> ae) aps_and_aes in
        collect (List.rev_append (ae1::aes) aes) (List.rev_append cs u)
      end
    end

  and collect_operator_contraints aexprs u = function
    | ABinaryOp (op,l,r,t) -> begin
      match op with
      | Plus
      | Minus
      | Mult
      | Divide
      | Mod    -> begin
        let (t1,t2) = (type_of l, type_of r) in
        collect (l::r::aexprs) ((t1, TInt)::(t2, TInt)::u)
      end
      | And
      | Or     -> begin
        let (t1,t2) = (type_of l, type_of r) in
        collect (l::r::aexprs) ((t1,TBool) :: (t2,TBool) :: u)
      end
      | Gt
      | Lt
      | Ltq
      | Gtq
      | Eq
      | Neq    -> begin
        let (t1,t2) = (type_of l, type_of r) in
        collect (l::r::aexprs) ((t1,t2) :: u)
      end
    end
    | AUnaryOp (op,ae,t) -> begin
      match op with
      | Not   -> begin
        let t1 = type_of ae in
        collect (ae :: aexprs) ((t1,TBool) :: u)
      end
    end
    | _ as ae -> begin
      let msg = Printf.sprintf
        "The expression: %s\n is not an operator, but an operator was expected."
        (aexpr_to_string ae) in
      runtime_error msg
    end

(* collect the constraints and perform unification *)
  let infer (e : expr) (fvs : (id, typ) Hashtbl.t) : typ =
    let ae = annotate e fvs in
    debug_print "annotated expression:";
    debug_print (aexpr_to_string ae);
    let cl = collect [ae] [] in
    debug_print "constraints:";
    debug_print (constraints_to_string cl);
    let s = Unify.unify cl in
    debug_print "substitution:";
    debug_print (subst_to_string s);
    let t = (Unify.apply s (type_of ae)) in
    debug_print "type before alpha varying:";
    debug_print (type_to_string t);
    reset_type_vars ();
    alpha_vary t
end

module UnifySol = struct
  open Ast
  open Exceptions
  open Printer

(* check if a variable occurs in a term *)
  let rec occurs (x : id) (t : typ) : bool =
    match t with
    | TBool
    | TUnit
    | TInt -> false
    | TList t -> occurs x t
    | TVar y -> x = y
    | Arrow (u, v) -> occurs x u || occurs x v

(* substitute term s for all occurrences of var x in term t *)
  let rec subst (s : typ) (x : id) = function
    | TBool -> TBool
    | TInt -> TInt
    | TUnit -> TUnit
    | TList t'  -> TList (subst s x t')
    | TVar y as t -> if x = y then s else t
    | Arrow (u, v) -> Arrow (subst s x u, subst s x v)

(* apply a substitution to t right to left *)
  let apply (s : substitution) (t : typ) : typ =
    List.fold_right (fun (x, e) -> subst e x) s t

(* unify one pair *)
  let rec unify_one (s : typ) (t : typ) : substitution =
    match (s, t) with
    | (TVar x, TVar y) -> if x = y then [] else [(x, t)]
    | (Arrow (x, y), Arrow (u, v)) -> unify [(x, u); (y, v)]
    | ((TVar x, (Arrow (u, v) as z)) | ((Arrow (u, v) as z), TVar x)) ->
      if occurs x z
      then circular s t
      else [(x, z)]
    | (TVar x, (TList _ as z))
    | ((TList _ as z), TVar x) ->
      if occurs x z
      then circular s t
      else [(x, z)]
    | (TBool, TVar x)
    | (TVar x, TBool) -> [(x,TBool)]
    | (TInt, TVar x)
    | (TVar x, TInt) -> [(x, TInt)]
    | (TUnit, TVar x)
    | (TVar x, TUnit) -> [x,TUnit]
    | (TList u, TList v) -> unify [(u,v)]
    (* already handled all cases involving TVar *)
    | _ -> begin
      if s = t then [] 
      else begin 
	Printf.printf "(%s) <> (%s)\n" (type_to_string s) (type_to_string t);
	unify_error s t
      end
    end

(* unify a list of constraints *)
  and unify (s : constr list) : substitution =
    match s with
    | [] -> []
    | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2
end

let timed f = Assertions.timeout 2 f ()

exception Test_failed of string
let run_test i j =
  let test_str = List.nth  (List.nth levels (i-1)) j in
  let test = parse_expression test_str in
  let type_context = Hashtbl.create 16 in
  timed begin fun () ->
    let ae = InferSol.annotate test type_context in
    let constraints = InferSol.collect [ae] [] in
    let student_type =
      Unify.apply (Unify.unify constraints) (InferSol.alpha_vary (InferSol.type_of ae)) in
    let sol_type = List.nth (List.nth  solutions (i-1)) j in
    let subst_to_sol =
      UnifySol.unify [student_type, sol_type] in
    Printf.printf
      "Student: %s\n Solution: %s\n subst: %s\n\n"
      (Printer.type_to_string student_type)
      (Printer.type_to_string sol_type)
      (Printer.subst_to_string subst_to_sol);
    match
        sol_type = student_type
    with
    | true -> ()
    | false -> begin
      let stu_str = Printer.type_to_string student_type in
      let sol_str = Printer.type_to_string sol_type in
      raise (Test_failed (Format.sprintf "Test: %s Got '%s', expected '%s'" test_str stu_str sol_str))
    end
  end

TEST_UNIT "unify1-1" = run_test 1 0
TEST_UNIT "unify1-2" = run_test 1 1
TEST_UNIT "unify1-3" = run_test 1 2
TEST_UNIT "unify1-4" = run_test 1 3
TEST_UNIT "unify1-5" = run_test 1 4
TEST_UNIT "unify1-6" = run_test 1 5
TEST_UNIT "unify1-7" = run_test 1 6
TEST_UNIT "unify1-8" = run_test 1 7
TEST_UNIT "unify1-9" = run_test 1 8
TEST_UNIT "unify1-10" = run_test 1 9
TEST_UNIT "unify2-1" = run_test 2 0
TEST_UNIT "unify2-2" = run_test 2 1
TEST_UNIT "unify2-3" = run_test 2 2
TEST_UNIT "unify2-4" = run_test 2 3
TEST_UNIT "unify2-5" = run_test 2 4
TEST_UNIT "unify2-6" = run_test 2 5
TEST_UNIT "unify2-7" = run_test 2 6
TEST_UNIT "unify2-8" = run_test 2 7
TEST_UNIT "unify2-9" = run_test 2 8
TEST_UNIT "unify2-10" = run_test 2 9
TEST_UNIT "unify3-1" = run_test 3 0
TEST_UNIT "unify3-2" = run_test 3 1
TEST_UNIT "unify3-3" = run_test 3 2
TEST_UNIT "unify3-4" = run_test 3 3
TEST_UNIT "unify3-5" = run_test 3 4
TEST_UNIT "unify3-6" = run_test 3 5
TEST_UNIT "unify3-7" = run_test 3 6
TEST_UNIT "unify3-8" = run_test 3 7
TEST_UNIT "unify3-9" = run_test 3 8
TEST_UNIT "unify3-10" = run_test 3 9
TEST_UNIT "unify4-1" = run_test 4 0
TEST_UNIT "unify4-2" = run_test 4 1
TEST_UNIT "unify4-3" = run_test 4 2
TEST_UNIT "unify4-4" = run_test 4 3
TEST_UNIT "unify4-5" = run_test 4 4
TEST_UNIT "unify4-6" = run_test 4 5
TEST_UNIT "unify4-7" = run_test 4 6
TEST_UNIT "unify4-8" = run_test 4 7
TEST_UNIT "unify4-9" = run_test 4 8
TEST_UNIT "unify4-10" = run_test 4 9
TEST_UNIT "unify5-1" = run_test 5 0
TEST_UNIT "unify5-2" = run_test 5 1
TEST_UNIT "unify5-3" = run_test 5 2
TEST_UNIT "unify5-4" = run_test 5 3
TEST_UNIT "unify5-5" = run_test 5 4
TEST_UNIT "unify5-6" = run_test 5 5
TEST_UNIT "unify5-7" = run_test 5 6
TEST_UNIT "unify5-8" = run_test 5 7
TEST_UNIT "unify5-10" = run_test 5 9
TEST_UNIT "unify6-1" = run_test 6 0
TEST_UNIT "unify6-2" = run_test 6 1
TEST_UNIT "unify6-3" = run_test 6 2
TEST_UNIT "unify6-4" = run_test 6 3
TEST_UNIT "unify6-5" = run_test 6 4
TEST_UNIT "unify6-6" = run_test 6 5
TEST_UNIT "unify6-7" = run_test 6 6
TEST_UNIT "unify6-8" = run_test 6 7
TEST_UNIT "unify6-9" = run_test 6 8
TEST_UNIT "unify6-10" = run_test 6 9
TEST_UNIT "unify6-11" = run_test 6 10
TEST_UNIT "unify7-1" = run_test 7 0
TEST_UNIT "unify7-2" = run_test 7 1
TEST_UNIT "unify7-3" = run_test 7 2
(* TEST_UNIT "unify7-4" = run_test 7 3 *)
(* TEST_UNIT "unify7-5" = run_test 7 4 *)
(* TEST_UNIT "unify7-6" = run_test 7 5 *)
(* TEST_UNIT "unify7-7" = run_test 7 6 *)
(* TEST_UNIT "unify7-8" = run_test 7 7 *)
(* TEST_UNIT "unify7-10" = run_test 7 9 *)
TEST_UNIT "unify8-1" = run_test 8 0
TEST_UNIT "unify8-2" = run_test 8 1
TEST_UNIT "unify8-3" = run_test 8 2
TEST_UNIT "unify8-4" = run_test 8 3
TEST_UNIT "unify8-5" = run_test 8 4
TEST_UNIT "unify8-6" = run_test 8 5
TEST_UNIT "unify8-7" = run_test 8 6
(* TEST_UNIT "unify8-8" = run_test 8 7 *)
(* TEST_UNIT "unify8-10" = run_test 8 9 *)
TEST_UNIT "unify9-1" = run_test 9 0
TEST_UNIT "unify9-2" = run_test 9 1
TEST_UNIT "unify9-3" = run_test 9 2
(* TEST_UNIT "unify9-4" = run_test 9 3 *)
(* TEST_UNIT "unify9-5" = run_test 9 4 *)
(* TEST_UNIT "unify9-6" = run_test 9 5 *)
(* TEST_UNIT "unify9-7" = run_test 9 6 *)
(* TEST_UNIT "unify9-8" = run_test 9 7 *)
(* TEST_UNIT "unify9-10" = run_test 9 9 *)
TEST_UNIT "unify10-1" = run_test 10 0
TEST_UNIT "unify10-2" = run_test 10 1
TEST_UNIT "unify10-3" = run_test 10 2
(* TEST_UNIT "unify10-4" = run_test 10 3 *)
(* TEST_UNIT "unify10-5" = run_test 10 4 *)
(* TEST_UNIT "unify10-6" = run_test 10 5 *)
(* TEST_UNIT "unify10-7" = run_test 10 6 *)
(* TEST_UNIT "unify10-8" = run_test 10 7 *)
(* TEST_UNIT "unify10-9" = run_test 10 8 *)
(* TEST_UNIT "unify10-10" = run_test 10 9 *)
