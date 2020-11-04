
module Make(F : Numbers.OrderedField) = struct

open F

type point   = number * number
type polygon = point list
type region  = Region.Make(F).region

let minkowski_difference_convex obstacle poly =
  failwith "TODO: Istanbul"

let minkowski_difference obstacles poly =
  failwith "TODO: Constantinople"

end
