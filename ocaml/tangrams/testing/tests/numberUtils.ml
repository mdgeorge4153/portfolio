open ReleaseTypes

module QuotientUtils (Q : Quotient) = struct
  open Q
  let (<>) x y = not (x === y)
end

module GroupUtils (G : Group) = struct
  include QuotientUtils (G)
  open G
  let (-) x y = x + (-y)
end

module RingUtils (R : Ring) = struct
  include GroupUtils (R)
  open R
  let rec number_of_int x =
    if Pervasives.(<) x 0 then -number_of_int (Pervasives.(~-) x)
    else if x = 0
      then R.zero
      else let lower_bit   = if x mod 2 = 0 then zero else one in
	   let higher_bits = number_of_int (x / 2) in
	   higher_bits + higher_bits + lower_bit

  (* also useful *)
  let two = one + one
  let three = one + two
end

module FieldUtils (F : Field) = struct
  include RingUtils (F)
  open F
  let (/) x y = x * inv y
end

module OrderedRingUtils (R : OrderedRing) = struct
  include RingUtils (R)
  open R

  let (>=) a b = is_non_neg (a - b)
  let (>)  a b = a >= b && a <> b
  let (<)  a b = not (a >= b)
  let (<=) a b = not (a > b)

  let min x y = if x >= y then y else x
  let max x y = if x >= y then x else y

  (* OrderedType *)
  type t = number
  let compare x y =
    if x < y then -1
    else if x === y then 0
    else (* x > y *) 1

  (* also useful *)
  let abs n = if is_non_neg n then n else -n

end

module OrderedFieldUtils (F : OrderedField) = struct
  include FieldUtils (F)
  include OrderedRingUtils (F)
end

