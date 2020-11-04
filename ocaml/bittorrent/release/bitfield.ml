open Core.Std

type 'a t = bool array

type readonly
type writeable

let length bitfield = Array.length bitfield

let is_set bitfield index = bitfield.(index)

let create_empty num_fields = Array.create ~len:num_fields false

let copy = Array.copy

let imp_set bitfield index = bitfield.(index) <- true

let imp_clear bitfield index = bitfield.(index) <- false

let imp_union basefield bitfield =
  if length basefield <> length bitfield then
    failwith "bitfields should be the same length"
  else
    Array.iteri
      ~f:(fun i e -> if e then basefield.(i) <- true else ()) bitfield

let imp_diff basefield bitfield =
  if length basefield <> length bitfield then
    failwith "bitfields should be the same length"
  else
    Array.iteri
      ~f:(fun i e -> if e && bitfield.(i) then basefield.(i) <- false else ())
      basefield

let set bitfield index =
  let bits = copy bitfield in
  imp_set bits index; bits

let clear bitfield index =
  let bits = copy bitfield in
  imp_clear bits index; bits

let union basefield bitfield =
  let bits = copy basefield in
  imp_union bits bitfield; bits

let diff basefield bitfield =
  let bits = copy basefield in
  imp_diff bits bitfield; bits

let all_true bitfield =
  Array.for_all bitfield ~f:(fun e -> e)

let all_false bitfield =
  Array.for_all bitfield ~f:(fun e -> not e)

let num_true bitfield =
  Array.count bitfield ~f:(fun e -> e)

let subset basefield bitfield =
  try
    Array.for_all2_exn basefield bitfield ~f:(fun a b -> b || not a)
  with
  | _ -> failwith "bitfields should be the same length"
