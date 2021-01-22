(** Module for fetching html pages from the web and parsing them *)

open Async

(******************************************************************************)
(** {2 URLs}                                                                  *)
(******************************************************************************)

type url

val url_to_string : url    -> string
val string_to_url : string -> url option

(******************************************************************************)
(** {2 Pages}                                                                 *)
(******************************************************************************)

type page = {
  url : url;
  title : string;      (** The document title *)
  links : url list;    (** The list of outgoing links *)
  words : string list; (** The list of words in the page *)

  descr : string list  (** A list of ``descriptive'' words about the document,
                          such as words from the document title and
                          descsription meta tags *)
}

(** Fetch and return a page. *)
val fetch_page : url -> page option Deferred.t

