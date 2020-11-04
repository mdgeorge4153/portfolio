
#use "region.ml";;
module F  = Numbers.Rationals
module FU = Numbers.OrderedFieldUtils(F)
module R  = Make (F)

open F
open FU
open R

#install_printer format;;

let pair_eq eq (x1,y1) (x2,y2) = eq x1 x2 && eq y1 y2

let (=!) = pair_eq (===)

let four = two + two

let mtl map = PointMap.fold (fun k v acc -> (k,v)::acc) map []
let contain_to_str con =
  match con with
    | Boundary -> "Boundary"
    | Inside -> "Inside"
    | Outside -> "Outside"

module TestData = struct
  let zed  = zero
  let two  = one + one
  let half = one / two

  let p1 = zed, zed
  let p2 = one, zed
  let p3 = two, two
  let p4 = zed, one

  let q1 = one,  one
  let q2 = zed, -one
  let q3 = one, -one
  let q4 = two,  one

  let r1 =  half,  half
  let r2 = -half,  half
  let r3 = -half, -half
  let r4 =  half, -half

  (* p and q intersect at i1 and i2
   * q and r intersect at i1 and i3
   * p and r intersect at i1 and i4
   *)
  let i1 = half, zero
  let i2 = one  + half, one
  let i3 = half * half, -half
  let i4 = zero, half

  (* the regions *)
  let l1 = [p1; p2; p3; p4]
  let l2 = [q1; q2; q3; q4]
  let l3 = [r1; r2; r3; r4]
  let p = create l1
  let q = create l2
  let r = create l3

  (* these are the expected edges for the region p *)
  let e21 = {target = p1; left_in = false; right_in = true}
  let e32 = {target = p2; left_in = false; right_in = true}
  let e34 = {target = p4; left_in = true;  right_in = false}
  let e41 = {target = p1; left_in = true;  right_in = false}

  (* these are for comparison testing *)
  let edge p = {target = p; left_in = false; right_in = false}
  let e'1 = edge (-one,zed)
  let e'2 = edge (zero,half)



  (* here is the expected output from p union q *)
  let pqdata = [
    q2, [];
    p1, [];
    p4, [{target=p1; right_in=false; left_in=true}];
    i1, [{target=p1; right_in=true;  left_in=false};
         {target=q2; right_in=false; left_in=true}];
    q3, [{target=q2; right_in=true;  left_in=false}];
    i2, [];
    p3, [{target=p4; right_in=false; left_in=true};
         {target=i2; right_in=true;  left_in=false}];
    q4, [{target=i2; right_in=false; left_in=true};
         {target=q3; right_in=true;  left_in=false}];
  ]

  (* here is the expected output from p union q union r *)
  let pqrdata = [
    r3, [];
    r2, [{target=r3; right_in=false; left_in=true}];
    q2, [];
    i4, [{target=r2; right_in=false; left_in=true}];
    p4, [{target=i4; right_in=false; left_in=true}];
    i3, [{target=r3; right_in=true;  left_in=true};
         {target=q2; right_in=false; left_in=true}];
    i1, [];
    q3, [{target=q2; right_in=true;  left_in=false}];
    i2, [];
    p3, [{target=p4; right_in=false; left_in=true};
         {target=i2; right_in=true;  left_in=false}];
    q4, [{target=i2; right_in=false; left_in=true};
         {target=q3; right_in=true;  left_in=false}];
  ]

  let pqregion = List.fold_left (fun acc (k,v) -> PointMap.add k v acc) PointMap.empty pqdata 
  let pqrregion = List.fold_left (fun acc (k,v) -> PointMap.add k v acc) PointMap.empty pqrdata 
end

open TestData

let test_adj_pairs = List.for_all2 (pair_eq (=!))
                                   (adjacent_pairs l1)
                                   [(p1, p2); (p2, p3); (p3, p4); (p4, p1)]

let test_compare_1 = compare_points p1 p2 = Lt
let test_compare_2 = compare_points p1 p1 = Eq
let test_compare_3 = compare_points p2 p1 = Gt
let test_compare_4 = compare_points p1 p4 = Lt
let test_compare_5 = compare_points p4 p1 = Gt

let test_locate_2 = locate (two, zed)  p = Outside
let test_locate_3 = locate (half, zed) p = Boundary

let test_compare_around_1 = compare_slopes_from p3 p2 p4 = Gt
let test_compare_around_2 = compare_slopes_from p3 p4 p2 = Lt
let test_compare_around_3 = compare_slopes_from p2 p1 (-one,zero) = Eq
let test_compare_around_4 = compare_slopes_from p4 p1 (zero,half) = Eq
let test_compare_around_5 = compare_slopes_from p4 p1 (-one,zero) = Gt
let test_compare_around_6 = compare_slopes_from r1 i4 i1 = Lt

let test_binary_search =
  let open Pervasives in
  binary_search (fun x -> if 2 < x then Lt else if 2 = x then Eq else Gt)
                [| 0; 1; 2; 2; 2; 3; 3; 4; 5 |]
  = (2,5)

let test_compare_edge_to_point_1 = compare_edge_to_point r4 (p2, edge p1) = Gt
let test_compare_edge_to_point_2 = compare_edge_to_point r1 (p2, edge p1) = Lt
let test_compare_edge_to_point_3 = compare_edge_to_point i1 (p2, edge p1) = Eq

let test_edges = binary_search (compare_edge_to_point i1)
                               [| (p3, edge p4);
                                  (r1, edge i1);
                                  (p2, edge p1);
                                  (q3, edge q2) |]
               = (1,3)


let test_segment_ix = segment_intersect (i1, edge q2) (r4, edge r3)

let test_closest_on_1 = closest_point_on r4 (p3, p2) =! p2
let test_closest_on_2 = closest_point_on p3 (q1, q2) =! q1
let test_closest_on_3 = closest_point_on r1 (p2, p1) =! i1

let dq = r2 -! r4
let dp = q2 -! i1
let d1 = i1 -! r4

let test_contains_n = contains (one + one, zero) p = false

(** Test from tangrams game ***************************************************)

module TangramTest = struct

  (* these coordinates come from taking the mink. sum of the square with the two
   * small triangles; they cause the drag and drop to crash. *)
  let x7  = -(number_of_int 7)  / four
  let x11 = -(number_of_int 11) / four
  let x15 = -(number_of_int 15) / four
  let x19 = -(number_of_int 19) / four

  let y1  = one
  let y2  = half
  let y3  = zero
  let y4  = -three/two
  let y5  = -two
  let y6  = -(number_of_int 5) / two

  let p1 = x19, y6
  let p2 = x19, y2
  let p3 = x15, y2
  let p4 = x7,  y6
  let p5 = x7,  y4

  let q1 = x19, y3
  let q2 = x19, y1
  let q3 = x11, y5
  let q4 = x7,  y5
  let q5 = x7,  y1

  let p = create [p1; p4; p5; p3; p2]
  let q = create [q1; q3; q4; q5; q2]

  let test_contains = locate (smult half (p5 +! p3)) q
end

module TangramsTest2 = struct

  let n_of (x,y) = number_of_int x, number_of_int y

  let p1  = n_of (  1, -9)
  let p2  = n_of (  1,  7)
  let p3  = n_of (  2,  6)
  let p4  = n_of (  2,  8)
  let p5  = n_of (  6,  8)
  let p6  = n_of (  9, -9)
  let p7  = n_of ( 12, -4)
  let p8  = n_of ( 13, -5)
  let p9  = n_of ( 14, -4)
  let p10 = n_of ( 14,  0)

  let p = create [p1; p6; p8; p7; p9; p10; p5; p4; p3; p2]

  let q1  = n_of (  3, -9)
  let q2  = n_of (  3, -1)
  let q3  = n_of (  7,  3)
  let q4  = n_of ( 11,  3)
  let q5  = n_of ( 15, -9)
  let q6  = n_of ( 19, -5)

  let q = create [q1; q5; q6; q4; q3; q2]

end

open TangramsTest2

(*
** vim: ts=2 sw=2 et ai
*)
