open Async.Std

module Job = struct
  type input  = int list
  type key    = int
  type inter  = int
  type output = int list

  let name = "tm.job"

  (* *)
  let map input =
    match input with
      | num::lst -> begin
          return (List.fold_left (fun a x -> (x, num)::a) [] lst)
        end
      | _ -> failwith "invalid"

  let reduce (_, counts) =
    return counts
end

(* Register the Job *)
let () = MapReduce.register_job (module Job)


module App  = struct
  let name = "tm"

  (** Print out the counts of [l], sorted by count *)
  let output l =
    let rec generate n =
      if n <= 10 then n::(generate (n+1)) else [] in
    let validate a (x, lst) =
      if a then List.sort (fun a b -> a - b) lst = generate x else false in
    if (List.fold_left validate true l) then
      printf "success!\n" else printf "failure!\n"

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (** args is interpreted as a list of filenames; the words in all listed
        files are counted. *)
    let main args =
      let rec generate n =
        if n > 0 then n::(generate (n-1)) else [] in
      let rec until n =
        if n > 0 then (n::(generate n))::(until (n-1)) else [] in
      MR.map_reduce (until 10) >>| output
  end
end

(* register the App *)
let () = MapReduce.register_app (module App)

