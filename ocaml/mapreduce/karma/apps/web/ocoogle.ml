open Async.Std

(** Web search application.  When run, crawls the web and then presents an
    interactive dialogue allowing the user to search. *)
module App = struct
  let name = "ocoogle"
  module Make (Controller : MapReduce.Controller) = struct

    module Crawler  = Crawler.Make(Controller)
    module Search   = Search.Make(Controller)
    module PageRank = PageRank.Make(Controller)
  
    let usage () =
      print_endline "Usage: ... ocoogle N seed1 [seed2 seed3 ...]";
      print_endline "";
      print_endline "  where N is a number and seed1 ... are urls";
      print_endline "";
      print_endline "  follows links from the seeds until it finds N pages and then";
      print_endline "  run an interactive search engine that searches those pages";
      ()

    (** run the application! *)
    let run_search n seeds =

      let urls = List.map (fun arg -> match Web.string_to_url arg with
          | None     -> failwith ("invalid url: "^arg);
          | Some url -> url
        ) seeds
      in

      print_endline "starting to crawl";
      Crawler.crawl urls n >>= fun pages ->
      printf "found %i urls\n" (List.length pages);
      print_endline "";

      print_endline "running page_rank";
      PageRank.page_rank pages >>= fun ranked_pages ->
      print_endline "page_rank complete";
      print_endline "";

      let stdin = Core.Std.Lazy.force Reader.stdin in
      let rec loop () =
        print_endline "enter a search string (or control-D to exit)";
        Reader.read_line stdin >>= function
          | `Eof      -> print_endline "EOF received, shutting down.";
                         shutdown 0;
                         return ()
          | `Ok query -> Search.search ranked_pages query >>= fun results ->
                         List.iteri (fun i page ->
                           printf "(#%i) %s\n" (1+i) page.Web.title;
                           printf "      %s\n"       (Web.url_to_string page.Web.url);
                           print_endline ""
                         ) results;
                         print_endline "";
                         loop ()
      in
      loop ()


    let main args =
      match args with
        | [] | [_] -> print_endline "Error: not enough arguments supplied!";
                      usage ();
                      return ()
        | n::seeds -> let n = int_of_string n in
                      run_search n seeds

  end
end

MapReduce.register_app (module App)


