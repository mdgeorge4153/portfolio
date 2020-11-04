(** Priority queue definitions and implementation *)

(** The signature for a priority queue type *)
module type PQ = sig

  (** a priority queue containing 'a *)
  type 'a t

  (** constructs an empty priority queue, ordered by the given comparator *)
  val empty    : 'a Util.comparator -> 'a t

  (** [insert x q] yields a queue containing [x] and the elements of [q] *)
  val insert   : 'a -> 'a t -> 'a t

  (** [remove q] returns the maximum element of [q]
      (according to [comparator q]) and another queue [q'] containing
      the remaining elements

      It returns None if the queue is empty.  *)
  val remove   : 'a t -> ('a * 'a t) option

  (** [max q] returns the maximum element of [q] (according to [comparator q])
      returns None if the queue is empty. *)
  val max      : 'a t -> 'a option

  (** [size q] returns the number of elements of [q] *)
  val size     : 'a t -> int

  (** [is_empty q] returns true if [q] is empty *)
  val is_empty : 'a t -> bool

  (** [comparator q] returns the comparator that [q] is ordered by *)
  val comparator : 'a t -> 'a Util.comparator

end

(** {2 Priority Queue Implementations} *)

(** A priority queue implementation based on sorted lists *)
module ListImpl : PQ

(** A priority queue implementation using a binary heap *)
module HeapImpl : PQ

(** {2 Heapsort } *)

(** A heapsort implementation using the given priority queue implementation *)
module Heapsort (PQ : PQ) : sig

  (** see List.sort *)
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list

end
