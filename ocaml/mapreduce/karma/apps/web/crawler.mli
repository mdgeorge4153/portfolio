open Async

module Make (C : MapReduce.Controller) : sig

  (** [crawl seeds n] follows links starting from the [seeds],
      until it has fetched at least [n] pages. *)
  val crawl : Web.url list -> int -> Web.page list Deferred.t
end

