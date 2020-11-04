(** Utility functions for problem set 3. *)

(******************************************************************************)
(** {2 timing}                                                                *)
(******************************************************************************)

(** [time f x] runs [f x] and returns the time it takes to run (in seconds). *)
val time : ('a -> 'b) -> 'a -> float

(******************************************************************************)
(** {2 randomization}                                                         *)
(******************************************************************************)

(** [rand_list n l] returns a random list of [l] integers between [0] and [n] *)
val rand_list : int -> int -> int list

(** randomly shuffle a list. *)
val shuffle : 'a list -> 'a list

(** randomly select an element from a list *)
val choice : 'a list -> 'a

(******************************************************************************)
(** {2 comparisons}                                                           *)
(******************************************************************************)

type comparison_result = Lt (** Less than *)
                       | Eq (** Equal to *)
                       | Gt (** Greater than *)

(** An 'a comparator represents a total order on 'as. *)
type 'a comparator = 'a -> 'a -> comparison_result

(******************************************************************************)
(** {2 printing}                                                              *)
(******************************************************************************)

(** [string_of_list f l] returns a pretty joining of [f] invoked on every
    element of [l]. *)
val string_of_list: ('a -> string) -> 'a list -> string

