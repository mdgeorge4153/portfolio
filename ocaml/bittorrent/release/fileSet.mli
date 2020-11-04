open Async.Std

(**
 * A FileSet.t represents a single fixed-sized block of data that is spread
 * across a fixed collection of fixed-sized files on disk.
 *
 * When reading from or writing to a FileSet.t, the user specifies an
 * integer position; the FileSet implementation translates this position
 * into the correct position of the correct file within the fileset.
 *)
type t

(**
 * The FileSet.entry type is used to specify the location and size of a
 * single file within the fileset.
 *)
type entry = {
  path : bytes;
  length : int64;
}

(**
 * Loads or creates a file set.  If none of the files exists, then they will be
 * created.  If they all exist and are of the right length, then they will be
 * opened.
 *
 * Raises an exception if only some of the files exist, or if they are the
 * incorrect length or cannot be opened for writing.
 *)
val load : entry list -> ([`Created | `Existing] * t) Deferred.t

(**
 * Returns the total length of a file set.  Position i is "in bounds" for
 * file set f if 0 <= i < length f
 *)
val length : t -> int64

(**
 * Queues the given data to be written to the position in the file set.
 * Raises an exception if any of the data to be written is out of bounds for the
 * fileset.
 *)
val write : t -> pos:int64 -> bytes -> unit Deferred.t

(**
 * Returns the data from the given position in the file set.  Raises an
 * exception if any of the positions to be read are out of bounds for the
 * fileset.
 *
 * Read guarantees that any data written to the positions being read is flushed
 * before returning.
 *)
val read : t -> pos:int -> len:int -> bytes Deferred.t

