
module Make(F : Numbers.OrderedField) = struct

open F
module FU = Numbers.OrderedFieldUtils(F)
open FU
module R = Region.Make(F)

type point   = number * number
type polygon = point list
type region  = R.region
let (@) l1 l2 = List.rev_append (List.rev l1) l2
let eq_p (a,b) (c,d) : bool = a === c && b === d

let minkowski_difference_convex (obstacle:polygon) (poly:polygon) : polygon =
    (* Get the minkowski difference, no duplicates *)
    let points =
        let fold_fun acc (a,b) = 
            let inner_fun acc2 (c,d) =
                let new_p = (a-c, b-d) in
                if List.exists (eq_p new_p) acc then acc2 else new_p::acc2 in
            acc @ (List.fold_left inner_fun [] poly) in
        List.fold_left fold_fun [] obstacle in

    (* Get the bottom-left most point in lst *)
    let rec left lst = 
        match lst with
        | [] -> failwith "mink_diff_convex: left: list empty"
        | h::t -> 
            let fold_fun (a,b) (c,d) =
                if a < c then (a,b)
                else if a === c then 
                    if b < d then (a,b) else (c,d)
                else (c,d) in
            List.fold_left fold_fun (List.hd lst) (List.tl lst) in

    (* Three utility functions for vectors *)
    let cross (a,b) (c,d) : number = (a*d - b*c) in
    let diff (a,b) (c,d)  : point  = (a-c, b-d)  in
    let sq_length (a,b)   : number = a*a + b*b   in

    (* Calculate the convex hull of points l, given a starting point p *)
    (* This is Gift Wrapping going CCW *)
    let rec convex_hull (l:polygon) (p:point) (acc:polygon) = 
        let fold_fun acc x =
            let dx   = diff x p in
            let dacc = diff acc p in
            if cross dacc dx < zero || eq_p dacc (zero,zero) ||
               (cross dacc dx === zero && (sq_length dx) > (sq_length dacc))
            then x
            else acc in
        let new_p =
            match l with
            | h::t -> List.fold_left fold_fun h t
            | _ -> failwith "convex_hull l less than 1 point!" in
        match acc with
        | [] -> convex_hull l new_p (acc@[new_p])
        | h::t -> if eq_p h new_p then acc
                  else convex_hull l new_p (acc@[new_p]) in

    match points with
    | [] -> []
    | h::t -> convex_hull points (left points) []

(* Folds over the obstacles, using union to merge mink. differences *)
let minkowski_difference (obstacles:polygon list) (poly:polygon) : region =
    let fold_fun acc x = (R.create (minkowski_difference_convex x poly))::acc in
    let diffs = List.fold_left fold_fun [] obstacles in
    List.fold_left (fun acc x -> R.union acc x) (R.create []) diffs

end
