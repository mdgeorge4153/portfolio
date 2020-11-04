type comparison_result = Lt | Eq | Gt

type 'a comparator = 'a -> 'a -> comparison_result

let time f x =
  let start = Sys.time () in
  ignore(f x); Sys.time () -. start

let rand_list n l =
  Array.to_list (Array.init l (fun _ -> Random.int n))

let shuffle l = 
  let rec take l n = match l with
    | []               -> failwith "empty list"
    | h::tl when n = 0 -> h, tl
    | h::tl            -> let x, rest = take tl (n - 1) in
                          x, h::rest
  in
  let choose l = take l (Random.int (List.length l)) in
  let rec helper taken left = match left with
    | [] -> taken
    | _  -> let h,tl = choose left in
            helper (h::taken) tl
  in
  helper [] l

let choice l =
  List.nth l (Random.int (List.length l))

let string_of_list f l =
  let rec sol f l acc = 
    match l with
    | []    -> acc
    | x::xs -> sol f xs (acc ^ ", " ^ (f x))
  in
  "[" ^ (sol f l " ") ^ "]"

