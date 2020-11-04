(** Library for an asynchronous queue. *)

open Async.Std

type 'a t

val make : unit -> 'a t

val enqueue : 'a t -> 'a -> unit

val dequeue : 'a t -> 'a Deferred.t
