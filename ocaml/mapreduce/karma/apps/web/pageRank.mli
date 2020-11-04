open Async.Std

type rank = float

module Make (C : MapReduce.Controller) : sig
  
  (** return an association list from pages to their ranks. *)
  val page_rank : Web.page list -> (Web.page * rank) list Deferred.t

end
