open Async

type body = {
  mass : float;        (** mass in kg *)

  position : Vector.t; (** position in meters *)

  velocity : Vector.t; (** velocity in meters / sec *)
}

val g : Vector.scalar

val read_data        : string -> body list Deferred.t
val write_transcript : string -> body list list -> unit Deferred.t

