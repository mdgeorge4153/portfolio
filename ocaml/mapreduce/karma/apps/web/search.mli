open Async

module Make (C : MapReduce.Controller) : sig

  (** return a short highly ranked list of pages that match the given query. *)
  val search : (Web.page * PageRank.rank) list -> string
            -> Web.page list Deferred.t

end
