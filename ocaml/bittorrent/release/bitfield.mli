(**
 * A Bitfield.t represents a collection of boolean values indexed by the
 * integers [0,1,2...,n-1].  You can think of it as a compact representation
 * of a bool array.
 *
 * The Bitfield API supports both mutable and immutable bit fields, using a
 * programming idiom called "phantom types".  The Bitfield.t type is
 * parameterized by a type called the mode, which should be either "readonly"
 * or "writeable".
 *
 * An immutable bitfield is represented by a "readonly Bitfield.t" while a
 * mutable bitfield is represented by a "writeable Bitfield.t".  Bitfield
 * operations that do not imperatively update the bitfield apply to an 'a
 * Bitfield.t, so they can be used with either mutable or immutable bitfields,
 * while the imperative operations "imp_read" and "imp_write" can only be applied
 * to "writeable Bitfield.t"'s.
 *)
type 'a t

type readonly
type writeable

val length : 'a t -> int

(** Returns true if the index of the bitfield has been set *)
val is_set : 'a t -> int  -> bool

val create_empty : int -> 'a t

val set   : 'a t -> int  -> 'c t
val clear : 'a t -> int  -> 'c t
val union : 'a t -> 'b t -> 'c t
val diff  : 'a t -> 'b t -> 'c t (* a - b *)
val copy  : 'a t         -> 'c t

val imp_set   : writeable t -> int  -> unit
val imp_clear : writeable t -> int  -> unit
val imp_union : writeable t -> 'a t -> unit
val imp_diff  : writeable t -> 'a t -> unit (* a <- a - b *)

val all_true  : 'a t -> bool
val all_false : 'a t -> bool
val num_true  : 'a t -> int
val subset    : 'a t -> 'b t -> bool (* a is a subset of b *)
