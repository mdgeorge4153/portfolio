open Async.Std

type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

(**
 * Indicates that a function can either result in a Failure,
 * during which there will be no value, or a Success,
 * which will be associated with the desired value.
 *)
type 'a can_fail =
  [`Failure | `Success of 'a]

(**
 * Reads the contents that the Reader.t provides and translates it into a BExpr.t.
 * This function can return a `Failure if the contents of the Reader.t
 * do not map to a BExpr.t as expected.
 *)
val decode : Reader.t -> t can_fail Deferred.t

(**
 * Parses a BExpr.t into its string representation.
 *)
val encode : t -> string
