module F  = Numbers.Rationals
module FU = Numbers.OrderedFieldUtils(F)
module Stud = Geometry.Make(F)
module Sol = Geometry_sol.Make(F)
module R = Region.Make(F)
open F
open FU
open R

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

  let s1 = two+two, zero
  let s2 = two+one, -one
  let s3 = two+two, -one

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
  let lref = [s1;s2;s3]

  let num_to_str num =
      format Format.str_formatter num;
      Format.flush_str_formatter ()
  
  let print_pt pt = 
      let (x,y) = Sol.point_to_num pt in
      let xstr = num_to_str x in
      let ystr = num_to_str y in
      Printf.printf "(%s,%s) " xstr ystr

  let print_lst lst = List.map print_pt lst

  let ch_test_1 () =
      print_endline "Convex Hull Test 1:";
      let sol = Sol.minkowski_difference_convex l1 lref in
      let stu = Stud.minkowski_difference_convex l1 lref in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_2 () =
      print_endline "Convex Hull Test 2:";
      let sol = Sol.minkowski_difference_convex l2 lref in
      let stu = Stud.minkowski_difference_convex l2 lref in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_3 () =
      print_endline "Convex Hull Test 3:";
      let sol = Sol.minkowski_difference_convex l3 lref in
      let stu = Stud.minkowski_difference_convex l3 lref in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_4 () =
      print_endline "Convex Hull Test 4:";
      let sol = Sol.minkowski_difference_convex l3 l1 in
      let stu = Stud.minkowski_difference_convex l3 l1 in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_5 () =
      print_endline "Convex Hull Test 5:";
      let sol = Sol.minkowski_difference_convex l3 l2 in
      let stu = Stud.minkowski_difference_convex l3 l2 in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_6 () =
      print_endline "Convex Hull Test 6:";
      let sol = Sol.minkowski_difference_convex l3 l3 in
      let stu = Stud.minkowski_difference_convex l3 l3 in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_7 () =
      print_endline "Convex Hull Test 7:";
      let sol = Sol.minkowski_difference_convex l2 l3 in
      let stu = Stud.minkowski_difference_convex l2 l3 in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_8 () =
      print_endline "Convex Hull Test 8:";
      let sol = Sol.minkowski_difference_convex l1 l2 in
      let stu = Stud.minkowski_difference_convex l1 l2 in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

  let ch_test_fancy () =
      print_endline "Convex Hull Test fancy:";
      let sq1 = zero,-two in
      let sq2 = one, -two in
      let sq3 = one, -one in
      let sq4 = zero, -one in
      let tr1 = zero, zero in
      let tr2 = one, zero in
      let tr3 = one, one in
      let sq = [sq1; sq2; sq3; sq4] in
      let tr = [tr1;tr2;tr3] in
      let sol = Sol.minkowski_difference_convex sq tr in
      let stu = Stud.minkowski_difference_convex sq tr in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

 let diff_test_1 () =
      print_endline "Difference Test 1:";
      let sol = vertices (Sol.minkowski_difference [l1;l2;l3] lref) in
      let stu = vertices (Stud.minkowski_difference [l1;l2;l3] lref) in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

 let diff_test_2 () =
      print_endline "Difference Test 2:";
      let sol = vertices (Sol.minkowski_difference [l1;l2;l3] [s1]) in
      let stu = vertices (Stud.minkowski_difference [l1;l2;l3] [s1]) in
      print_endline "Instructor: ";
      let _ = print_lst sol in
      print_endline "\nStudent: ";
      let _ = print_lst stu in
      print_endline "\n"

 let main () =
     ch_test_1 (); 
     ch_test_2 ();
     ch_test_3 ();
     ch_test_6 ();
     ch_test_8 ();
     ch_test_fancy ();
     diff_test_1 ();
     diff_test_2 ();;

 main ();;
