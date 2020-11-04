
module Make (F : Numbers.OrderedField) = struct

module FU = Numbers.OrderedRingUtils(F)
open F
open FU

module G = Geometry.Make(F)
open G

module R = Region.Make(F)
open R

type point   = F.number * F.number
type polygon = point list

(* Point Functions and other useful things *)
let sum_points (a,b) (c,d) : point = (a+c,b+d)
let dif_points (a,b) (c,d) : point = (a-c,b-d)
let move_shape s del : polygon =
    List.fold_right (fun x acc -> (sum_points x del)::acc) s []
let origin : point = (zero,zero)
let neg (a,b) : point  = (~-a, ~-b)

(* Ref Variables *)
let shapes : polygon list ref = ref []
let shape  : polygon ref = ref []
let start_p : point ref = ref origin
let mink_region : region ref = ref (create [])

let create_shape p =
    shapes := p :: !shapes

(* Refs are initialized *)
let click (p:point) : unit  =
    let (conts, rest) =
        List.partition (fun x -> contains p (create x)) !shapes in
    match conts with
    | h::_ -> shapes := rest;
              shape := h;
              start_p := p;
              let temp_s = move_shape h (neg p) in
              mink_region := (minkowski_difference rest temp_s);
    | [] -> ()

(* Refs changed *)
let move_to (p:point) : unit  =
    let close_p = find_closest !mink_region p in
    shape := move_shape !shape (dif_points close_p !start_p);
    start_p := close_p

(* Refs reset *)
let unclick () =
    create_shape !shape; shape := []

let obstacles () = !shapes

let selection () = 
    match !shape with
    | [] -> None
    | _ -> Some(!shape)

let extra_points () = []

let extra_lines ()  = []
end
