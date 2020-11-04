open Async.Std

module Make (C : MapReduce.Controller) = struct

  let rec take n l = match n,l with
    | _, []    -> []
    | 0, l     -> []
    | _, h::tl -> h :: (take (n - 1) tl)

  let search pages query =
    let ranked_result  = take 9 pages in
    let result  = List.map fst ranked_result in
    let warnurl = match Web.string_to_url "http://WARNING.com/WARNING" with
      | Some u -> u | None -> failwith "can't parse URL!"
    in
    let warning = {
                    Web.url   = warnurl;
                    Web.title = "WARNING: search is unimplemented, returning arbitrary pages";
                    Web.links = [];
                    Web.words = [];
                    Web.descr = [];
                  }
    in
    return (warning::result)

end

