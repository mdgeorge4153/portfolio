open Async.Std

(**
 * The BlockSet module is responsible for splitting a file into
 * multiple pieces, each of which may be split into multiple blocks.
 * The set of pieces is predetermined and cannot change, and each piece has
 * an intended hash.
 *
 * Users can write new blocks into a BlockSet.  When all of the blocks that make
 * up a piece have been written, the BlockSet will check the that the entire
 * piece matches the desired hash, and will then indicate that the block is
 * present by determining the "has p" deferred.
 *
 * All of the functions will fail if passed a block_id or a piece_id that are
 * out of bounds for the block set.
 *)
type t

(**
 * The index of a piece in the file.  The pieces of a file are always numbered
 * 0, 1, ..., n
 *)
type piece_id = int

(** A block is a subset of a piece *)
type block_id = {
  piece  : piece_id;

  (* the first byte of the block, relative to the first byte of the piece. *)
  offset : int;

  (* the length of the block *)
  length : int;
}

(**
 * Create a block set using the filename and hashes from the given.  The block
 * set may not be empty, if the file already exists and was partially or wholly
 * complete.
 *)
val create : Metainfo.t -> t Deferred.t

(**
 * a list of all of the piece numbers in the BlockSet (whether the set
 * has them or not).  This is the same as [0; 1; ...; num_pieces t - 1]
 *)
val all_pieces : t -> piece_id list

(**
 * the number of pieces in the set (whether they have been written or not)
 *)
val num_pieces : t -> int

(**
 * the list of pieces that are not yet in the BlockSet
 *)
val need       : t -> piece_id list

(**
 * A bitfield of length [num_pieces t] with bit p set if piece p has been
 * written and verified.
 *)
val contents   : t -> Bitfield.readonly Bitfield.t

(**
 * [has s p] becomes determined when all of the blocks of the piece p have
 * been written and the hash of the entire piece has been verified.
 *)
val has      : t -> piece_id -> unit Deferred.t

(**
 * Write the given block into the set.
 *)
val write    : t -> block_id -> bytes -> unit Deferred.t

(**
 * Read the given block
 *)
val read : t -> block_id -> bytes Deferred.t

(**
 * Return the underlying file set
 *)
val fileset  : t -> FileSet.t

(**
 * Return the underlying metainfo
 *)
val metainfo : t -> Metainfo.t

(** [failed_to_verify bs] returns the asynchronous failure queue for this [t].
    Whenever a block fails to verify, its [piece_id] is added to the queue.

    Note that multiple calls to [failed_to_verify] will return the same
    queue. *)
val failed_to_verify : t -> piece_id AQueue.t

(** [blocks_of_piece bs id] returns the list of blocks contained in the piece with
    ID [id].
    Assumes a 16KB block size. *)
val blocks_of_piece : t -> piece_id -> block_id list

(** return the total number of bytes that are still required. *)
val bytes_left : t -> int64

