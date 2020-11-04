(*
BEGIN FILECONTENTS .depend
async
END FILECONTENTS
BEGIN FILECONTENTS .opam_packages
core
async
END FILECONTENTS
*)

open Warmup
open Async.Std

exception Assertion_error

let assert_equal a b : unit =
  if not (a = b) then raise Assertion_error

TEST_UNIT "deferred_map on int list" =
  ( deferred_map [1;2;3;4;5] (fun i -> return (i+1)) ) >>>
  (fun l -> assert_equal l [2;3;4;5;6])

TEST_UNIT "deferred_map on empty list" =
  ( deferred_map [] (fun i -> failwith "whoops") ) >>>
  (fun l -> assert_equal l [])

TEST_UNIT "deferred_map on string list" =
  ( deferred_map ["hello";"world"] (fun s -> return ("3110" ^ s)) ) >>>
  (fun l -> assert_equal l ["3110hello";"3110world"])

let _ = ignore (Thread.create (fun () -> Scheduler.go ()) ());
  Thread.delay 1.0 (* timeout *)
