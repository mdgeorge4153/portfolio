open Numbers
(* is_non_neg
one
zero
+
-
~
*
inv
===
create
approximate
pi
e *)
module R = Rationals
let their_one = R.one
let our_one = R.create(fun x -> Rationals.one)
let their_zero = R.zero
let our_zero = R.create(fun x -> Rationals.zero)

module intUtils = RingUtils (Integers)
let piTop = intUtils.number_of_int 3141592653589793238462643383279502884197169399375105820974944592307816
let piBottom = intUtils.number_of_int 1000000000000000000000000000000000000000000000000000000000000000000000
let eTop = intUtils.number_of_int 2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391
let eBottom = intUtils.number_of_int 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
let toFrac x y = R.( * ) (R.from_numerator x) (R.inv (R.from_numerator y))
let truePi = toFrac piTop piBottom
let trueE = toFrac eTop eBottom