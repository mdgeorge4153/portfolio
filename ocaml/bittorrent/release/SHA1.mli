(** Contains functions for working with SHA1 hashes. *)

(**
 * A SHA1.t represents a 20 byte SHA1 hash. SHA1.ts can be compared using (=).
 *)
type t

(** Compute the SHA1 hash of the given byte sequence. *)
val hash : bytes -> t

(** Convert a SHA1 hash to a byte string. Length of the result is always 20. *)
val to_bytes : t -> bytes

(** of_bytes b converts a bytes representation of a SHA1.t into the SHA1.t type. *)
val of_bytes : bytes -> t
