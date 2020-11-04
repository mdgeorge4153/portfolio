open Async.Std

(******************************************************************************)
(** {2 The WebCount Job}                                                      *)
(******************************************************************************)

module Job = struct
  type input
  type key
  type inter
  type output

  let name = "webcount.job"

  let map input =
    failwith "TODO: witty failwith"

  let reduce (key,inters) =
    failwith "TODO: witty failwith"
end

(* Register the Job *)
let () = MapReduce.register_job (module Job)


(******************************************************************************)
(** {2 The WebCount App}                                                      *)
(******************************************************************************)

module App  = struct
  let name = "webcount"

  (** Print out the counts of [l], sorted by count *)
  let output l =
    let print (word,count) =
      printf "%-15s %i\n%!" ("\""^word^"\":") count
    in
    let sorted = List.sort (fun (_,v1) (_,v2) -> v1 - v2) l in
    List.iter print sorted

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    let usage () =
      print_endline "Usage: ... webcount N seed1 [seed2 seed3 ...]";
      print_endline "";
      print_endline "  where N is a number and seed1 ... are urls";
      print_endline "";
      print_endline "  follows links from the seeds until it finds N pages and then";
      print_endline "  reports a summary of the words on those pages";
      ()

    (** args is interpreted as a list of uris; 
	files are counted. *)
    let main args =
      usage ();
      return ()
  end
end

(* register the App *)
let () = MapReduce.register_app (module App)

