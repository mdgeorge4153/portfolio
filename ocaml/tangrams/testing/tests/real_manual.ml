#load "nums.cma";;
#use "numbers.ml";;
#install_printer Rationals.format;;
module R = Reals;;
#install_printer R.format;;


module IntUtils = RingUtils(Integers);;
(* let piTop = intUtils.number_of_int 3141592653589793238462643383279502884197169399375105820974944592307816;;
let piBottom = intUtils.number_of_int 1000000000000000000000000000000000000000000000000000000000000000000000;;
let eTop = intUtils.number_of_int 2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391;;
let eBottom = intUtils.number_of_int 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;;
let toFrac x y = R.( * ) (R.from_numerator x) (R.inv (R.from_numerator y));;
let truePi = toFrac piTop piBottom;;
let truePiR = fun x -> truePi;;
let trueE = toFrac eTop eBottom;;
let trueER = fun x -> trueE;; *)
let one = R.one;;
let zero = R.zero;;
let minus_one = R.(~-) R.one;;
let print_bool x = if x then print_endline "true" else print_endline "false";;

let test_neg () =
  print_endline "actual:true";
  print_bool (R.is_non_neg one);
  print_endline "actual:false";
  print_bool (R.is_non_neg minus_one);
  print_endline "should not terminate";
  R.is_non_neg zero

let test_eq () = 
  print_endline "actual:false";
  print_bool (R.( ===) one zero);
  print_endline "should not terminate";
  R.( === ) one one

let test_plus () = 
  (R.( + ) one minus_one, R.( + ) one zero , R.( + ) one one)

let test_mult () = 
  (R.( * ) one minus_one, R.( * ) one zero, R.( * ) one one)


let test_inv () = 
  Rationals.float_of_number (R.approximate (R.inv (R.inv R.e)) 10)

let test_inv2 () =
  Rationals.float_of_number (R.approximate (R.inv (R.inv R.pi)) 3)

let test_pi () =
  R.float_of_number R.pi

let test_e () = 
  R.float_of_number R.e

let test_pi2 () =
  Rationals.float_of_number (R.approximate R.pi 3)

let test_e2 () = 
  Rationals.float_of_number (R.approximate R.e 10) 

