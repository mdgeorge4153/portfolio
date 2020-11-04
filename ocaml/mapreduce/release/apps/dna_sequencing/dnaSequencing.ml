open Async.Std
open Async_unix

(******************************************************************************)
(** input and output types                                                    *)
(******************************************************************************)

type id = int
type dna_type = Read | Ref

type sequence = {
  id   : id;
  kind : dna_type;
  data : string;
}

(** Indicates a matching subsequence of the given read and reference *)
type result = {
  length   : int;

  read     : id;
  read_off : int;

  ref      : id;
  ref_off  : int;
}

(******************************************************************************)
(** file reading and writing                                                  *)
(******************************************************************************)

(** Convert a line into a sequence *)
let read_sequence line = match Str.split (Str.regexp "@") line with
  | [id; "READ"; seq] -> {id=int_of_string id; kind=Read; data=seq}
  | [id; "REF";  seq] -> {id=int_of_string id; kind=Ref;  data=seq}
  | _ -> failwith "malformed input"

(** Read in the input data *)
let read_files filenames : sequence list Deferred.t =
  if filenames = [] then failwith "No files supplied"
  else
    Deferred.List.map filenames Reader.file_lines
      >>| List.flatten
      >>| List.map read_sequence


(** Print out a single match *)
let print_result result =
  printf "read %i [%i-%i] matches reference %i [%i-%i]\n"
         result.read result.read_off (result.read_off + result.length - 1)
         result.ref  result.ref_off  (result.ref_off  + result.length - 1)

(** Write out the output data *)
let print_results results : unit =
(* note: cmp not in sp2014 release. TODO: remove this note *)
  let cmp x y =
    let c1 = compare x.read y.read in
    let c2 = compare x.ref y.ref in
    let c3 = compare x.read_off y.read_off in
    let c4 = compare x.ref_off y.ref_off in
    if c1 != 0 then c1 else if c2 != 0 then c2 else if c3 != 0 then c3 else c4
  in
  List.iter print_result (List.sort cmp results)

(******************************************************************************)
(** Dna sequencing jobs                                                       *)
(******************************************************************************)

module Job1 = struct
  type input
  type key
  type inter
  type output

  let name = "dna.job1"

  let map input : (key * inter) list Deferred.t =
    failwith "Heavy decibels are playing on my guitar / We got deferred dot ts coming up through the code"

  let reduce (key, inters) : output Deferred.t =
    failwith "We're just listening to the rock that's giving too much noise / Are you deaf you wanna hear some more?"
end

let () = MapReduce.register_job (module Job1)



module Job2 = struct
  type input
  type key
  type inter
  type output

  let name = "dna.job2"

  let map input : (key * inter) list Deferred.t =
    failwith "Well, I asked you if you wanted any memory and refs / You said you wanted functional data types instead"

  let reduce (key, inters) : output Deferred.t =
    failwith "We're just talking about the future / Forget about the past / I'll always stay functional / It's never gonna segfault, never gonna segfault"
end

let () = MapReduce.register_job (module Job2)



module App  = struct

  let name = "dna"

  module Make (Controller : MapReduce.Controller) = struct
    module MR1 = Controller(Job1)
    module MR2 = Controller(Job2)

    let run (input : sequence list) : result list Deferred.t =
      failwith "Rock 'n roll ain't noise pollution / Rock 'n' roll ain't gonna die / Rock 'n' roll ain't noise pollution / Rock 'n' roll it will survive"

    let main args =
      read_files args
        >>= run
        >>| print_results
  end
end

let () = MapReduce.register_app (module App)

