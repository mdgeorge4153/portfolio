open Async.Std

(** The input is a list of URIs, each of which is either new or old.  The new
    urls are fetched and traversed.  The complete set of URIs are returned, with
    the new new URIS (i.e.  the URIS that were not contained in the input) are
    marked as New. *)
module Crawler = struct

  type task = Todo of Web.url | Done of Web.page | Fail of Web.url

  type input  = task list
  type key    = Web.url
  type inter  = task
  type output = task

  let name = "crawler.job"

  (* passes on the current item and if it needs processing, also passes along
     new tasks *)
  let map_one task = match task with

    (* just pass through completed jobs *)
    | Done {Web.url=u}
    | Fail u -> return [u,task]

    | Todo url ->
        Web.fetch_page url >>= function
          | Some page ->
              let new_tasks = List.map (fun u -> u,Todo u) page.Web.links in
              return ((url, Done page)::new_tasks)
          | None ->
              return [url, Fail url]

  let map tasks =
    Deferred.List.map tasks ~how:`Parallel ~f:map_one >>| List.flatten

  (* if there is a done, return it, otherwise return a Todo *)
  let reduce (url,tasks) =
    let url_task = List.fold_left (fun a t -> match t with
        | Done p -> Done p
        | Fail u -> Fail u
        | Todo u -> a
      ) (Todo url) tasks
    in return url_task

  (** post condition: the returned list is "closed" under links *)
  let pages tasks =
    let blank_page url = {
      Web.url   = url;
      Web.title = "<unknown title>";
      Web.links = [];
      Web.words = [];
      Web.descr = [];
    } in

    List.map (function
      | Done p -> p
      | Fail u -> blank_page u
      | Todo u -> blank_page u
    ) tasks

end

MapReduce.register_job (module Crawler)

module Make (C : MapReduce.Controller) = struct

  module CrawlerController  = C (Crawler)

  let batch n l =
    let add_one (k,results) x =
      if k = 0 then (n, [x]::results)
      else match results with
        | []    -> (k-1, [[x]])
        | h::tl -> (k-1, (x::h)::tl)
    in snd (List.fold_left add_one (0,[]) l )

  let crawl seeds npages =
    let rec loop i last_size pages =
      printf "starting crawl iteration %i\n" i;
      let n = List.length pages in
      if n > npages then begin
        (* we have enough *)
        printf "found %i pages, returning %i of them\n" n n;
        return pages
      end
  
      else if n <= last_size then begin
        (* we can't find any more *)
        printf "previous iteration produced no new pages.  Returning %i pages\n" n;
        return pages
      end
  
      else
        (* perform a crawl *)
        CrawlerController.map_reduce (batch 10 pages)
          >>| List.map snd
          >>= loop (succ i) n
    in
  
    let seeds = List.map (fun u -> Crawler.Todo u) seeds in
    loop 1 0 seeds >>| Crawler.pages

end
