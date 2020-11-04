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

  include RingProperties(R)
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
end

module type NiceField = sig
  include OrderedField
  include (NiceRing with type number := number)
end

(** Exercise 1 ****************************************************************)

module type IntsType = NiceRing
(* TODO: module Ints : IntsType = ... *)

module type IntegersType = NiceRing
(* TODO: module Integers : IntegersType = ... *)

module type FloatsType = NiceField
(* TODO: module Floats : FloatsType  = ... *)

module type Root23Type = sig
  include NiceRing
  val sqrt2 : number
  val sqrt3 : number
  val sqrt6 : number
end
(* TODO: module Root23 : Root23Type = ... *)

module type FieldOfFractionsType = functor (R : NiceRing) -> sig
  include NiceField

  val from_numerator : R.number -> number

  (* refines NiceField.inv; raises an exception if the argument is zero. *)
  val inv : number -> number
end
(* TODO: module FieldOfFractions : FieldOfFractionsType = ... *)

module type RationalsType = NiceField
(* TODO: module Rationals : RationalsType = ... *)

module type Rot15Type = sig
  include NiceField
  val cos45 : number
  val sin45 : number
  val cos30 : number
  val sin30 : number
end
(* TODO: module Rot15 : Rot15Type = ... *)

(** Exercise 2 ****************************************************************)

module QuotientUtils (Q : Quotient) = struct
  let (<>) x y = failwith "TODO: I'm your only friend"
end

module GroupUtils (G : Group) = struct
  include QuotientUtils (G)
  let (-) x y = failwith "TODO: well not your only friend"
end

module RingUtils (R : Ring) = struct
  include GroupUtils (R)
  let number_of_int n = failwith "TODO: but I'm a little glowing friend"
end

module FieldUtils (F : Field) = struct
  include RingUtils (F)
  let (/) x y = failwith "TODO: but really I'm not actually your friend"
end

module OrderedRingUtils (R : OrderedRing) = struct
  include RingUtils (R)
  let (<)  x y = failwith "TODO: but I am"
  let (>)  x y = failwith "TODO: blue canary in the attic by the light switch"
  let (<=) x y = failwith "TODO: who watches over you"
  let (>=) x y = failwith "TODO: make a little birdhouse in your soul"

  (* return the smaller of x and y under the (<) ordering *)
  let min  x y = failwith "TODO: Not to put to fine a point on it"

  (* return the larger of x and y under the (<) ordering *)
  let max  x y = failwith "TODO: say I'm the only bee in your bonnet"

  (* implement the Set.OrderedType.compare interface *)
  let compare x y = failwith "TODO: make a little birdhouse in your soul"
end

module OrderedFieldUtils (F : OrderedField) = struct
  include FieldUtils (F)
  include OrderedRingUtils (F)
end

(** Exercise 3 ****************************************************************)

(* TODO: this implementation of Rationals  is here so that the provided
 * interface for Reals compiles; it should be removed when you implement the
 * Rationals above
 *)
module Rationals = struct type number = unit end

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
(* TODO: module Reals : RealsType = ... *)

(******************************************************************************)

(*
** vim: ts=2 sw=2 ai et
*)
