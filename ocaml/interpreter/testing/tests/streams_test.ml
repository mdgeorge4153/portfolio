open Streams
open Assertions

(*
 * We test the stream implementations without using QCheck, since generating
 * an arbitrary function is impossible, and a stream is essentially just
 * a function.
 *)

let () = Random.init 3110

let timed f = Assertions.timeout 2 f ()

let rec take_sol n (Stream (h,t)) =
  if n = 0 then []
  else h::take_sol (n-1) (t ())

let rec unfold_sol f s =
  let (x,s') = f s in Stream (x, fun () -> unfold_sol f s')

let nats = unfold_sol (fun x -> x, succ x) 0
let nats_lst = take_sol 10 nats

let (<..>) f g x = f (g x)

let fork_sol (f,g) x = f x, g x

let univ_sol (f,g) x = (unfold_sol <..> fork_sol) (f,g) x

let hd_sol (Stream(h,_)) = h
let tl_sol (Stream(_,t)) = t ()

let map_sol f = univ_sol (f <..> hd_sol, tl_sol)

let%test_unit "take1" = 
  timed begin fun () ->
    let n = 0 in 
    n === (List.length (take n nats))
  end

let%test_unit "take2" = 
  timed begin fun () ->
    let n = 1 in 
    n === (List.length (take n nats))
  end

let%test_unit "take3" = 
  timed begin fun () ->
    let rec test_range n s acc = 
      if n <= 0 then acc 
      else let acc' = (n = (List.length (take n s))) && acc in 
      test_range (n-1) s acc' 
    in
    assert_true (test_range 1001 nats true)
  end

let%test_unit "repeat1" = 
  timed begin fun () -> 
    let ones = repeat 1 in 
    [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1] === (take_sol 15 ones)
  end 

let%test_unit "repeat2" = 
  timed begin fun () -> 
    let a = repeat 'a' in 
    ['a';'a';'a';'a';'a';'a';'a';'a';'a';'a'] === (take_sol 10 a)
  end 

let%test_unit "repeat3" = 
  timed begin fun () -> 
    let tru = repeat true in 
    [true;true;true;true;true] === (take_sol 5 tru)
  end 

let%test_unit "repeat4" =
  (* Tests using repeat to create the stream [2;2;2;2;...] *)
  timed begin fun () ->
    let two_list = take_sol 10 (Streams.repeat 2) in 
    (List.fold_left (fun acc x -> acc && (x = 2)) true two_list) === true
  end

let%test_unit "map1" =
  (* Tests using map to create the stream [0;2;4;6;8;...] *)
  timed begin fun () ->
    let evens = map (fun x -> x * 2) nats in
    (take_sol 10 evens) === [0;2;4;6;8;10;12;14;16;18]
  end

let%test_unit "map2" =
  (* Tests using map to create the stream ["0";"1";"2";...] *)
  timed begin fun () ->
    let strs = map string_of_int nats in
    (take_sol 5 strs) === ["0";"1";"2";"3";"4"]
  end

(* TODO aab227 : Might need some more tests for diag *)
let%test_unit "diag1" =
  (* Tests using diag to create the stream of squares [0;1;4;9;...]
   * by creating rows of naturals and multiplying each by its row index *)
  timed begin fun () ->
    let matrix = 
      map_sol (fun n -> map_sol (fun x -> n*x) nats) nats in
    let squares = take_sol 10 (diag matrix) in
      squares === [0;1;4;9;16;25;36;49;64;81]
  end

(* let%test_unit "diag2" = 
  timed begin fun () -> 
    let ones = repeat 1 in 
    let matrix = 
      map_sol (fun n -> map_sol (fun x -> n+x) ones) ones in 
    let 


  end  *)


let%test_unit "suffixes1" =
  (* Tests using suffixes to get the lower triangle of the nats x nats
   * matrix *)
  timed begin fun () ->
    List.iteri
    (fun i s ->
      Assertions.(===) (List.map (fun x -> x - i) (take 10 s)) nats_lst)
    (take_sol 10 (suffixes nats))
  end

let%test_unit "interleave1" =
  timed begin fun () -> 
    let ones = unfold_sol (fun x -> x, 1) 1 in 
    let twos = unfold_sol (fun x -> x, 2) 2 in
    let offset = interleave ones twos in 
    (take_sol 10 offset) === [1;2;1;2;1;2;1;2;1;2]
  end  

let%test_unit "interleave2" =
  (* Tests interleave by interleaving streams of even and odd numbers to
   * create the stream of natural numbers *)
  timed begin fun () ->
    let evens = map_sol (fun x -> x * 2) nats in
    let odds = map_sol (fun x -> x * 2 + 1) nats in
    let nats_std = interleave evens odds in
    (take_sol 10 nats_std) === nats_lst
  end 

let%test_unit "fibs1" =
  (* Tests fibs by taking 100 from the stream and checking that they satisfy
   * the Fibonacci recurrence *)
  timed begin fun () ->
    let fibs = List.tl (List.tl (take_sol 1000 (fibs ()))) in
    let (result,_,_) = List.fold_left
      (fun (r,a,b) x -> (r && (a + b = x), b, x)) (true,0,1) fibs in
    result === true
  end 

let%test_unit "fibs2" =
  (* Tests fibs by taking the first 10 from the stream *)
  timed begin fun () ->
    let fibs = Streams.fibs () in
    (take_sol 10 fibs) === [0;1;1;2;3;5;8;13;21;34]
  end

let%test_unit "pi1" =
  (* Tests pi by checking that the first 100 terms in the stream satisfy
   * the Taylor error bound *)
  timed begin fun () ->
    let real = atan 1.0 in
    let l = List.map (fun x -> x /. 4.) (take_sol 100 (Streams.pi ())) in
    let checks = List.mapi (fun i x ->
      abs_float (x -. real) <= 1. /. (2. *. (float_of_int i) +. 1.)) l in
    (List.fold_left (&&) true checks) === true
  end

let%test_unit "pi2" =
  (* Checks the first 5 elements from the pi stream, to ensure it isn't
   * [pi;pi;pi...] or something *)
  timed begin fun () ->
  let approx_equal l =
    let approx = [4.0; 2.66666; 3.46666; 2.895238] in
    let approx2 = [0.0; 4.0; 2.66666; 3.46666] in
    List.fold_left2
      (fun acc x y -> acc && (abs_float(x -. y) <= 1e-5)) true approx l
    || List.fold_left2
      (fun a x y -> a && (abs_float(x -. y) <= 1e-5)) true approx2 l in
    let l = take_sol 4 (Streams.pi ()) in
    assert_true (approx_equal l)
  end

let%test_unit "lookandsay1" =
  (* Checks the first 7 elements from the look and say stream *)
  timed begin fun () ->
    let correct =
    [[1]; [1;1]; [2;1]; [1;2;1;1]; [1;1;1;2;2;1];
     [3;1;2;2;1;1]; [1;3;1;1;2;2;2;1]] in 
      correct === (take_sol 7 (Streams.look_and_say ()))
  end
