open Async.Std

(** A imperative queue using a singly-linked list. For internal use only.
    Sshhh!  Could have used a two-list queue, but I don't see the point. *)
module Queue = struct

  module Node = struct

    type 'a t = {
      value : 'a;
      mutable next : 'a t option;
    }

  end

  exception Empty

  type 'a t = {
    mutable head : 'a Node.t option;
    mutable tail : 'a Node.t option;
    mutable length : int;
  }

  let make () =
    {head=None; tail=None; length=0}

  let enqueue q x =
    let node = {Node.value=x; next=None} in
    match q.tail with
    | None ->
      q.head <- Some node ;
      q.tail <- Some node ;
      q.length <- 1
    | Some n ->
      n.Node.next <- Some node ;
      q.length <- q.length + 1

  let dequeue q =
    match q.head with
    | None -> None
    | Some n ->
      let {Node.value; next} = n in
      let () =
        q.head <- next ;
        begin if next = None then
          q.tail <- None
        else () end ;
        q.length <- q.length - 1
      in Some value

  let dequeue_exn q =
    match dequeue q with
    | None -> raise Empty
    | Some x -> x

  let peek q =
    match q.head with
    | None -> None
    | Some n -> n.Node.value

  let length q = 
    q.length

end

type 'a t = {
  surplus : 'a Queue.t;
  deficit : 'a Ivar.t Queue.t;
}

let make q =
  {surplus=Queue.make (); deficit=Queue.make ()}

let enqueue q x =
  if Queue.length q.deficit > 0 then
    let i = Queue.dequeue_exn q.deficit in
    Ivar.fill i x
  else
    Queue.enqueue q.surplus x

let dequeue q =
  let i = Ivar.create () in
  let () =
    if Queue.length q.surplus > 0 then
      Ivar.fill i (Queue.dequeue_exn q.surplus)
    else
      Queue.enqueue q.deficit i
  in Ivar.read i
