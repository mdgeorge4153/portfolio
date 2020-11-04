open Async.Std

type rank = float

module Make (C : MapReduce.Controller) = struct

  let page_rank pages =
    let ranked  = List.map (fun p -> (p, 0.)) pages in
    let warnurl = match Web.string_to_url "http://WARNING.com/WARNING" with
      | Some u -> u | None -> failwith "can't parse URL!"
    in
    let warning = {
                    Web.url   = warnurl;
                    Web.title = "WARNING: pages are unranked";
                    Web.links = [];
                    Web.words = [];
                    Web.descr = [];
                  }, 1.
    in
    return (warning::ranked)

end

