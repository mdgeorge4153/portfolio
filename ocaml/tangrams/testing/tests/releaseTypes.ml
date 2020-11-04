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

module type IntegersType = NiceRing

module type FloatsType = NiceField

module type Root23Type = sig
  include NiceRing
  val sqrt2 : number
  val sqrt3 : number
  val sqrt6 : number
end

module type FieldOfFractionsType = functor (R : NiceRing) -> sig
  include NiceField

  val from_numerator : R.number -> number

  (* refines NiceField.inv; raises an exception if the argument is zero. *)
  val inv : number -> number
end

module type RationalsType = NiceField

module type Rot15Type = sig
  include NiceField
  val cos45 : number
  val sin45 : number
  val cos30 : number
  val sin30 : number
end

module type RealsType = sig
  include NiceField

  (* given a sequence f of rational approximations f(1) f(2) f(3) ... that
   * converges to a real number x, (create f) returns x.
   *
   * f is required to converge at a rate of 10^(-k).  That is, for all k,
   * |x - f(k)| < 10^(-k).  Behavior is completely unspecified if f does not
   * converge fast enough.
   *)
  val create      : (int -> Numbers.Rationals.number) -> number

  (* approximate x k produces a rational approximations of x that is accurate
   * to within 10^(-k).  In other words, |x - (approximate x k)| < 10^(-k)
   *)
  val approximate : number -> int -> Numbers.Rationals.number

  val pi : number
  val e  : number
end


(** Utils types ***************************************************************)

module type QUType = functor (Q : Numbers.Quotient) -> sig
  val (<>) : Q.number -> Q.number -> bool
end

module type GUType = functor (G : Numbers.Group) -> sig
  val (<>) : G.number -> G.number -> bool
  val (-)  : G.number -> G.number -> G.number
end

module type GeometryType = functor (F : Numbers.OrderedField) -> sig
  type point   = F.number * F.number
  type polygon = point list
  type region  = Region.Make(F).region

  val minkowski_difference_convex : polygon      -> polygon -> polygon
  val minkowski_difference        : polygon list -> polygon -> region
end

module type RUType = functor (R : Numbers.Ring) -> sig
  val (<>) : R.number -> R.number -> bool
  val (-)  : R.number -> R.number -> R.number
  val number_of_int : int -> R.number
end

module type FUType = functor (F : Numbers.Field) -> sig
  val (<>) : F.number -> F.number -> bool
  val (-)  : F.number -> F.number -> F.number
  val number_of_int : int -> F.number
  val (/) : F.number -> F.number -> F.number
end

module type ORUType = functor (R : Numbers.OrderedRing) -> sig
  val (<>) : R.number -> R.number -> bool
  val (-)  : R.number -> R.number -> R.number
  val number_of_int : int -> R.number
  val (<)  : R.number -> R.number -> bool
  val (>)  : R.number -> R.number -> bool
  val (<=) : R.number -> R.number -> bool
  val (>=) : R.number -> R.number -> bool

  (* return the smaller of x and y under the (<) ordering *)
  val min : R.number -> R.number -> R.number

  (* return the larger of x and y under the (<) ordering *)
  val max : R.number -> R.number -> R.number

  (* implement the Set.OrderedType.compare interface *)
  val compare : R.number -> R.number -> int
end

module type OFUType = functor (R : Numbers.OrderedField) -> sig
  val (<>) : R.number -> R.number -> bool
  val (-)  : R.number -> R.number -> R.number
  val number_of_int : int -> R.number
  val (<)  : R.number -> R.number -> bool
  val (>)  : R.number -> R.number -> bool
  val (<=) : R.number -> R.number -> bool
  val (>=) : R.number -> R.number -> bool
  val (/)  : R.number -> R.number -> R.number

  (* return the smaller of x and y under the (<) ordering *)
  val min : R.number -> R.number -> R.number

  (* return the larger of x and y under the (<) ordering *)
  val max : R.number -> R.number -> R.number

  (* implement the Set.OrderedType.compare interface *)
  val compare : R.number -> R.number -> int
end

module type GameType = functor (F : Numbers.OrderedField) -> sig

  type point   = F.number * F.number
  type polygon = point list

  (* create_shape is used to initialize the game state.  It is
   * only called before any of the other functions are, and it should only be
   * called with non-overlapping polygons.
   *)
  val create_shape : polygon -> unit

  (* these functions should update the state of the game in
   * response to the corresponding events.  They will be called
   * by the ui
   *)
  val click   : point -> unit
  val move_to : point -> unit
  val unclick : unit  -> unit

  (* these functions are called by the ui to figure out what to draw.
   * the extra_points and extra_edges functions are for debugging: the ui will
   * draw any extra points or lines returned by these functions
   *)
  val obstacles    : unit -> polygon list
  val selection    : unit -> polygon option
  val extra_points : unit -> point list
  val extra_lines  : unit -> (point * point) list
end

