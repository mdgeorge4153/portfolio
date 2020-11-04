
module Make (F : Numbers.OrderedField) = struct

type point   = F.number * F.number
type polygon = point list

let shapes : polygon list ref = ref []

let create_shape p =
  shapes := p :: !shapes

let click p    = print_endline "TODO: while you're at it"
let move_to p  = print_endline "TODO: leave the nightlight on"
let unclick () = print_endline "TODO: inside the birdhouse in your soul"

let obstacles () = print_endline "TODO: my name is "; !shapes

let selection () = print_endline "TODO: blue canary"; None

let extra_points () = []

let extra_lines ()  = []

end


