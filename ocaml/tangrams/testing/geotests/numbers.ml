(** Some abstract algebra for you *********************************************)

module type Quotient = sig
  type number

  val ( === )  : number -> number -> bool
end

module QuotientProperties (Q : Quotient) = struct
  open Q
  let symmetric  a b   = if a === b then b === a else true;;
  let transitive a b c = if a === b &&   b === c then a === c else true;;
  let reflexive  a     = a === a;;
end

module type Group = sig
  include Quotient

  val zero   : number
  val ( +  ) : number -> number -> number
  val ( ~- ) : number -> number
end

module GroupProperties (G : Group) = struct
  open G

  include QuotientProperties (G)
  let commutative a b   = a + b === b + a
  let associative a b c = a + (b + c) === (a + b) + c
  let identity    a     = a + zero === a
  let inverses    a     = a + (~- a) === zero
end

module type Ring = sig
  include Group

  val one   : number
  val ( * ) : number -> number -> number
end

module RingProperties (R : Ring) = struct
  open R

  include GroupProperties (R)
  let times_associative  a b c = (a * b) * c === a * (b * c)
  let times_distributive a b c = a * (b + c) === a * b + a * c
  let times_identity     a     = a * one === a
  let times_commutative  a b   = a * b === b * a
end

module type Field = sig
  include Ring

  (* return the multiplicative inverse of the argument.  Behavior is undefined
   * if the argument is zero.
   *)
  val inv : number -> number
end

module FieldProperties (F : Field) = struct
  open F
  
  include RingProperties (F)
  let times_inverse a = if a === zero then true else a * inv a === one
end

module type OrderedRing = sig
  include Ring

  val is_non_neg : number -> bool
end

module OrderedRingProperties (R : OrderedRing) = struct
  open R

  let minus_one_negative     = not (is_non_neg ~-one)
  let squares_non_negative a = is_non_neg (a*a)

  let non_negative_times a b = if   is_non_neg a && is_non_neg b
                               then is_non_neg (a*b) else true

  let non_negative_plus  a b = if   is_non_neg a && is_non_neg b
                               then is_non_neg (a+b) else true
end

module type OrderedField = sig
  include Field
  include OrderedRing with type number := number
end

module OrderedFieldProperties (F : OrderedField) = struct
  include FieldProperties(F)
  include OrderedRingProperties(F)
end

(******************************************************************************)

module type NiceRing = sig
  include OrderedRing

  val float_of_number : number -> float
  val format          : Format.formatter -> number -> unit
  val roughly_half    : number -> number
end

module type NiceField = sig
  include OrderedField
  include (NiceRing with type number := number)
end

(** Exercise 2 ****************************************************************)

module QuotientUtils (Q : Quotient) = struct
  open Q
  let (<>) x y = not (x === y)
end

module GroupUtils (G : Group) = struct
  include QuotientUtils (G)
  open G
  let (-) x y = x + (-y)
end

module RingUtils (R : Ring) = struct
  include GroupUtils (R)
  open R
  let rec number_of_int x =
    if Pervasives.(<) x 0 then -number_of_int (Pervasives.(~-) x)
    else if x = 0
      then R.zero
      else let lower_bit   = if x mod 2 = 0 then zero else one in
           let higher_bits = number_of_int (x / 2) in
           higher_bits + higher_bits + lower_bit

  (* also useful *)
  let two = one + one
  let three = one + two
end

module FieldUtils (F : Field) = struct
  include RingUtils (F)
  open F
  let (/) x y = x * inv y
end

module OrderedRingUtils (R : OrderedRing) = struct
  include RingUtils (R)
  open R

  let (>=) a b = is_non_neg (a - b)
  let (>)  a b = a >= b && a <> b
  let (<)  a b = not (a >= b)
  let (<=) a b = not (a > b)

  let min x y = if x >= y then y else x
  let max x y = if x >= y then x else y

  (* OrderedType *)
  type t = number
  let compare x y =
    if x < y then -1
    else if x === y then 0
    else (* x > y *) 1

  (* also useful *)
  let abs n = if is_non_neg n then n else -n

end

module OrderedFieldUtils (F : OrderedField) = struct
  include FieldUtils (F)
  include OrderedRingUtils (F)
end

(** Exercise 1 ****************************************************************)

module type IntsType = NiceRing
module Ints : IntsType = struct
  type number = int

  let ( === ) = Pervasives.( = )

  let zero    = 0
  let ( + )   = Pervasives.( + )
  let ( ~- )  = Pervasives.( ~- )

  let one     = 1
  let ( * )   = Pervasives.( * )
  
  let is_non_neg = (<=) 0

  let float_of_number = float_of_int
  let format f = Format.fprintf f "%d"
  let roughly_half x = x / 2
end

module type IntegersType = sig
  include NiceRing

  val to_big_int : number          -> Big_int.big_int
  val of_big_int : Big_int.big_int -> number
end

module Integers : IntegersType = struct
  open Big_int
  type number = big_int

  let (===) = eq_big_int

  let zero  = zero_big_int
  let (+)   = add_big_int
  let (~-)  = minus_big_int

  let one   = unit_big_int
  let two   = one + one
  let ( * ) = mult_big_int

  let is_non_neg n = sign_big_int n >= 0

  let float_of_number = float_of_big_int
  let format f n = Format.fprintf f "%s" (string_of_big_int n)
  let roughly_half n = div_big_int n two

  let to_big_int x = x
  let of_big_int x = x
end

module type FloatsType = NiceField
module Floats : FloatsType  = struct
  type number = float

  let (===) = (=)

  let zero  = 0.
  let (+)   = (+.)
  let (~-)  = (~-.)

  let one   = 1.
  let ( * ) = ( *. )

  let inv n = 1. /. n

  let is_non_neg n = n >= 0.

  let float_of_number x = x
  let format f = Format.fprintf f "%f"
  let roughly_half x = x /. 2.
end

module type Root23Type = sig
  include NiceRing
  val sqrt2 : number
  val sqrt3 : number
  val sqrt6 : number
end
module Root23 : Root23Type = struct

  open Integers
  module IU = OrderedRingUtils(Integers)
  open IU

  type number = Integers.number * Integers.number
              * Integers.number * Integers.number

  let  (===) (a1,a2,a3,a6) (b1,b2,b3,b6) =
    a1 === b1 && a2 === b2 && a3 === b3 && a6 === b6

  let thr   = one + two
  let six   = thr + thr
  let (-) a b = a + (- b)

  let  (+)   (a1,a2,a3,a6) (b1,b2,b3,b6) = a1+b1, a2+b2, a3+b3, a6+b6
  let  (~-)  (a1,a2,a3,a6)               = -a1, -a2, -a3, -a6
  let  ( * ) (a1,a2,a3,a6) (b1,b2,b3,b6) = 
    let ( * ) = Integers.( * ) in
    let ( + ) = Integers.( + ) in
        one*a1*b1 + two*a2*b2 + thr*a3*b3 + six*a6*b6,
        one*a1*b2 + one*a2*b1 + thr*a3*b6 + thr*a6*b3,
        one*a1*b3 + two*a2*b6 + one*a3*b1 + two*a6*b2,
        one*a1*b6 + one*a2*b3 + one*a3*b2 + one*a6*b1

  let is_non_neg_1 = is_non_neg

  (* returns true if a1 + a2√2 >= 0 *)
  let is_non_neg_2 (a1, a2) =
    let ( *  ) = Integers.( *  ) in
    let ( +  ) = Integers.( +  ) in
    let ( ~- ) = Integers.( ~- ) in
    match is_non_neg_1 a1, is_non_neg_1 a2 with
    | true,  true  -> true
    | true,  false -> is_non_neg_1 (  a1*a1 - two*a2*a2)
    | false, true  -> is_non_neg_1 (- a1*a1 + two*a2*a2)
    | false, false -> false

  (* returns true if a1 + a2√2 + a3√3 + a6√6 >= 0 *)
  let is_non_neg (a1,a2,a3,a6) =
    let ( * )  = Integers.( * )  in
    let ( + )  = Integers.( + )  in
    let ( ~- ) = Integers.( ~- ) in
    (* we write the input as (a1 + a2√2) + (a3 + a6√2)√3.
     * we then apply the same approach used for is_non_neg_2 above.
     *
     * we will write A1 for (a1 + a2√2) and A3 for (a3 + a6√2), so that
     * the input number is A1 + A3√3
     *)
    match is_non_neg_2(a1,a2), is_non_neg_2(a3,a6) with
      | true,  true  -> true
      | true,  false -> (* A1*A1 - 3*A3*A3 >= 0 *)
          is_non_neg_2 ( a1*a1 + two*a2*a2 - thr * a3*a3 - six*a6*a6
                       , two*a1*a2 - six*a3*a6
                       )
      | false, true  -> (* -A1*A1 + 3*A3*A3 >= 0 *)
          is_non_neg_2 ( -a1*a1 - two*a2*a2 + thr*a3*a3 + six*a6*a6
                       , -two*a1*a2 + six*a3*a6
                       )
      | false, false -> false

  let float_of_number (a1, a2, a3, a6) =
    float_of_number a1 +. (float_of_number a2)*.(sqrt 2.)
                       +. (float_of_number a3)*.(sqrt 3.)
                       +. (float_of_number a6)*.(sqrt 6.)

  let format ff (a1,a2,a3,a6) =
    let print_term (coeff : Integers.number) root leading =
      if (is_non_neg_1 coeff) && not (Integers.(===) coeff zero)
                            && not leading
      then Format.fprintf ff "+"
      else ();
      if Integers.(===) coeff zero
           then leading
      else if Integers.(===) coeff one
           then begin Format.fprintf ff "%s" root; false end
      else if Integers.(===) coeff (Integers.(~-) one)
           then begin Format.fprintf ff "-%s" root; false end
      else begin format ff coeff; Format.fprintf ff "%s" root; false end
    in

    let print_ones coeff =
      if Integers.(===) coeff zero then true
      else begin
        format ff coeff;
        false
      end
    in
    let leading = print_ones a1 in
    let leading = print_term a2 "√2" leading in
    let leading = print_term a3 "√3" leading in
    let leading = print_term a6 "√6" leading in
    if leading then Format.fprintf ff "0" else ()

  let roughly_half (a1, a2, a3, a6) = (roughly_half a1, roughly_half a2,
                                       roughly_half a3, roughly_half a6)

  let sqrt2 = zero,  one, zero, zero
  let sqrt3 = zero, zero,  one, zero
  let sqrt6 = zero, zero, zero,  one
  let one   = one,  zero, zero, zero
  let zero  = zero, zero, zero, zero

end

(** Root23 with extra karma ***************************************************)
module type Number = sig val n : int end
module Two   : Number = struct let n = 2 end
module Three : Number = struct let n = 3 end

module RootN (N : Number) (R : NiceRing) : sig
  include NiceRing
  val rootN      : number
  val from_coeff : R.number -> number
end = struct
  (* x1 + xn√n *)
  open R
  module RU = OrderedRingUtils(R)
  open RU

  type number = R.number * R.number
  let (===) (x1, xn) (y1, yn) = x1 === y1 && xn === yn

  let zero  = R.zero, R.zero
  let one   = R.one,  R.zero
  let rootN = R.zero, R.one
  let from_coeff x1 = x1, R.zero

  let n = number_of_int N.n

  let is_non_neg (x1, xn) =
    let x1pos, xnpos = is_non_neg x1, is_non_neg xn in
    match x1pos, xnpos with
      | true,  true  -> true
      | false, false -> false
      | true,  false -> x1 * x1 >= n * xn * xn
      | false, true  -> x1 * x1 <= n * xn * xn

  let ( *  ) (x1, xn) (y1, yn) = x1 * y1 + n * xn * yn, xn * y1 + x1 * yn
  let ( +  ) (x1, xn) (y1, yn) = x1 + y1, xn + yn
  let ( ~- ) (x1, xn)          = -x1, -xn

  let float_of_number (x1, xn) = float_of_number x1
                              +. float_of_number xn *. sqrt(float_of_int N.n)

  let format f (x1,xn) = Format.fprintf f "%a + (%a)√%i" format x1 format xn N.n
  let roughly_half (x1, xn) = roughly_half x1, roughly_half xn
end

module Root23Karma : Root23Type = struct
  module Root2 = RootN (Two) (Integers)
  include RootN (Three) (Root2)
  let sqrt2 = from_coeff Root2.rootN
  let sqrt3 = rootN
  let sqrt6 = sqrt2 * sqrt3
end
  
(** Fields of fractions *******************************************************)

module type FieldOfFractionsType = functor (R : NiceRing) -> sig
  include NiceField

  val from_numerator : R.number -> number
  val from_num_denom : R.number -> R.number -> number

  val numerator   : number -> R.number
  val denominator : number -> R.number

  (* refines NiceField.inv; raises an exception if the argument is zero. *)
  val inv : number -> number
end
module FieldOfFractions : FieldOfFractionsType = functor (R : NiceRing) ->
struct
  open R

  type numerator = R.number
  type number    = R.number * R.number

  let from_numerator n = n, one
  let from_num_denom n d =
    if d === zero then failwith "Division by zero" else n, d

  let numerator   (n,d) = n
  let denominator (n,d) = d

  let (===) (n1, d1) (n2, d2) = d2 * n1 === d1 * n2

  let zero = from_numerator zero
  let (+) (n1, d1) (n2, d2) = n1 * d2 + n2 * d1, d1 * d2
  let (~-) (n, d) = -n, d

  let ( * ) (n1, d1) (n2, d2) = n1 * n2, d1 * d2
  let one  = from_numerator one

  let inv (n, d) = if R.(===) d R.zero
                   then failwith "division by zero"
                   else d, n
  let is_non_neg (n, d) =
    let n_pos = is_non_neg n in
    let d_pos = is_non_neg d in
    (n_pos && d_pos) || (not n_pos && not d_pos)

  let rec float_of_number (n, d) =
    let nf = R.float_of_number n in
    let df = R.float_of_number d in
    if (nf >= max_float || nf <= min_float)
    && (df >= max_float || df <= min_float)
      then float_of_number (roughly_half n, roughly_half d)
      else nf /. df

  let format f (n,d) =
    format f n;
    Format.fprintf f "/";
    format f d;
    ()

  let roughly_half (n,d) = roughly_half n, d
end

module type RationalsType = sig
  include NiceField
  val from_numerator : Integers.number -> number
  val from_num_denom : Integers.number -> Integers.number -> number

  val numerator   : number -> Integers.number
  val denominator : number -> Integers.number
end
module UnreducedRationals : RationalsType = FieldOfFractions (Integers)

(* Rationals with extra karma *)
module ReducedRationals : RationalsType = struct
  include UnreducedRationals

  let reduce x =
    let open Integers in
    let open Big_int  in
    let n, d   = to_big_int (numerator x), to_big_int (denominator x) in
    let gcd    = gcd_big_int n d in
    let n', d' = div_big_int n gcd, div_big_int d gcd in
    from_num_denom (of_big_int n') (of_big_int d')

  let ( + ) x y = reduce (x + y)
  let ( * ) x y = reduce (x * y)
end

module Rationals = ReducedRationals

module type Rot15Type = sig
  include NiceField
  val cos45 : number
  val sin45 : number
  val cos30 : number
  val sin30 : number
end
module Rot15 : Rot15Type = struct
  include FieldOfFractions(Root23)

  let two   = (Root23.(+) Root23.one Root23.one)
  let half  = inv (from_numerator two)

  let cos45 = (from_numerator Root23.sqrt2) * half
  let sin45 = cos45
  let cos30 = (from_numerator Root23.sqrt3) * half
  let sin30 = half
end

(** Exercise 3 ****************************************************************)

module type RealsType = sig
  include NiceField

  (* given a sequence f of rational approximations f(1) f(2) f(3) ... that
   * converges to a real number x, (create f) returns x.
   *
   * f is required to converge at a rate of 10^(-k).  That is, for all k,
   * |x - f(k)| < 10^(-k).  Behavior is completely unspecified if f does not
   * converge fast enough.
   *)
  val create      : (int -> Rationals.number) -> number

  (* approximate x k produces a rational approximations of x that is accurate
   * to within 10^(-k).  In other words, |x - (approximate x k)| < 10^(-k)
   *)
  val approximate : number -> int -> Rationals.number

  val pi : number
  val e  : number
end
module Reals : RealsType = struct
  module Q  = Rationals
  module QU = OrderedFieldUtils(Q)

  open Q
  open QU

  (* in this implementation, ek always refers to 10^-k (similarly for eX) *)

  (* invariant: |x - (x j)| < ej *)
  type number = int -> Q.number

  (*
   * if f is an estimator of a real number x, and if
   *  Some (guess, error) = f k (10^-k),
   *
   * then it should be the case that
   *  |x - guess| < error
   *)
  type estimator = int -> Q.number -> (Q.number * Q.number) option

  let one_tenth = one / (number_of_int 10)

  (* returns min (10^-k, 1) *)
  let ten_to_the_minus k =
    let rec loop j acc =
      if Pervasives.(<=) j 0 then acc else loop (pred j) (acc * one_tenth)
    in loop k one

  (* converts an estimator to a number by searching for a good enough approx *)
  let search (estimate : estimator) = fun k ->
    let ek = ten_to_the_minus k in
    let rec loop j ej =
      match estimate j ej with
        | Some (guess, error) when error < ek -> guess
        | Some _ | None                       -> loop (succ j) (one_tenth * ej)
    in loop 0 one

  (* helper functions for taylor series *)
  let rec choose n k =
    if k = 0 || k = n then one
    else (choose (pred n) (pred k)) + (choose (pred n) k)

  let pow base exp =
    let base = number_of_int base in
    let rec loop exp =
      if exp = 0
        then one
        else base * (loop (pred exp))
    in loop exp

  let rec fact i =
    if Pervasives.(<=) i 1 then one
    else (number_of_int i) * fact (pred i)

  let choose n k =
    (fact n) / (fact k) / (fact (Pervasives.(-) n k))

  let sum term j = 
    let rec loop j acc =
      if Pervasives.(<) j 0 then acc else loop (pred j) (acc + term j)
    in loop j zero

  (* pi and e *)
  let pi_term n =
                   three * (choose Pervasives.(2 * n) n)
                                   /
                 ((pow 16 n) * (two * (number_of_int n) + one))

  let pi =
    let estimator j ej =
      Some (sum pi_term j, pi_term j)
    in search estimator

  let e =
    let rec term j     = inv (fact j) in
    let estimator j ej =
      Some (sum term j, term j)
    in search estimator

  (* OrderedField functions *)

  let (===) x y =
    let rec check j two_ej =
      if abs (x j - y j) > two_ej then false
      else check (succ j) (one_tenth * two_ej)
    in check 0 (one + one)

  let is_non_neg x =
    let rec check j ej =
      let xj = x j in
      if xj > ej       then true
      else if xj < -ej then false
      else check (succ j) (one_tenth * ej)
    in check 0 one

  let ( + ) x y =
    let plus_estimator j ej =
      Some ((x j) + (y j) , ej + ej)
    in search plus_estimator

  let ( * ) x y =
    let open Q  in
    let open QU in
    let times_estimator j ej =
      let xj = x j in
      let yj = y j in
      let xlower, xupper = xj - ej, xj + ej in
      let ylower, yupper = yj - ej, yj + ej in
      let bounds = [xlower * ylower; xlower * yupper;
                    xupper * ylower; xupper * yupper] in
      let xylower = List.fold_left min (List.hd bounds) (List.tl bounds) in
      let xyupper = List.fold_left max (List.hd bounds) (List.tl bounds) in
      Some (xj * yj, xyupper - xylower)
    in search times_estimator

  let inv x =
    let open Q  in
    let open QU in
    let inv_estimator j ej =
      let xj = x j in
      let xlower, xupper = xj - ej, xj + ej in
      if xj === zero || xlower === zero || xupper === zero
        then
          None (* we can't make a guess *)
        else
          let bound1, bound2 = inv xlower, inv xupper in
          let lower,  upper  = min bound1 bound2, max bound1 bound2 in
          Some (inv xj, upper - lower)
    in search inv_estimator

  let (~-)  n   = fun k -> -(n k)
  let zero      = fun k -> zero
  let one       = fun k -> one

  let float_of_number x = float_of_number (x 5)
  let format f x = Format.fprintf f "%5f" (float_of_number x)
  let roughly_half x = fun k -> roughly_half (x k)

  let create      x = x
  let approximate x = x


end

(******************************************************************************)

(*
** vim: ts=2 sw=2 ai et
*)
