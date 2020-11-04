(* This implementation taken from a student *)


module Make(F : Numbers.OrderedField) = struct
        
include Numbers.OrderedFieldUtils (F)
open F

type point   = number * number
type polygon = point list
type region  = Region.Make(F).region
module R = Region.Make(F)

let minkowski_difference_convex obstacle poly =
    let rec diff o p h res =
        match o, p with
        | [(x1, y1)], (x2, y2)::tl -> diff o tl h ((x1 - x2, y1 - y2)::res)
        | [(x1, y1)], [] -> diff (fst h) (snd h) ([],[]) res
        | hd::tl, _ -> diff [hd] p (tl, p) res
        | _ -> res in
    (* compare based on x coordinate, use y coordinate to break ties *)
    let cmp (x1, y1) (x2, y2) =
        let c = compare x1 x2 in
        if c = 0 then compare y1 y2 else c in
    let left (x1, y1) (x2, y2) (x, y) =
        let v = (((x2 - x1) * (y - y1)) - ((y2 - y1) * (x - x1))) in
        if (v > zero) then
            true
        else if v === zero then
            (* Points are collinear, true iff (x, y) is further from (x1, y1)
             * then (x2, y2) *)
             ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)) <
             ((x - x1) * (x - x1) + (y - y1) * (y - y1))
        else
            false in 
    let jarvis pts f =
        let (=) (x1, y1) (x2, y2) = (x1 === x2) && (y1 === y2) in
        let rec process hu pt =
            let leftmost ep cp =
                if ((left pt ep cp) && (not (pt = cp))) then cp else ep in
            let lp = List.fold_left leftmost pt pts in
            assert(not (List.exists ((=) pt) hu));
            if lp = f  then (pt::hu) else process (pt::hu) lp in
        process [] f in
    match List.sort cmp (diff obstacle poly ([],[]) []) with
    | hd::tl as pts -> jarvis pts hd
    | [] -> []

let minkowski_difference obstacles poly =
    let diff o = R.create (minkowski_difference_convex o poly) in
    match List.rev_map diff obstacles with
    | hd::tl -> List.fold_left R.union hd tl
    | [] -> failwith "Shouldn't happen"

let point_to_num (x,y) = (x,y)

end
