open Async.Std

type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

type 'a can_fail =
  [ `Failure | `Success of 'a]

let (>>==) v f =
  match v with
  | `Failure    -> `Failure
  | `Success v' -> f v'

let (>>=|) v f =
  match v with
  | `Failure    -> `Failure
  | `Success v' -> `Success (f v')

let return' v =
  return (`Success v)

let (>>===) (v : 'a can_fail Deferred.t) f =
  v >>= fun v2 ->
  match v2 with
  | `Failure    -> return `Failure
  | `Success v' -> f v'

let (>>==|) (v : 'a can_fail Deferred.t) f =
  v >>= fun v2 ->
  match v2 with
  | `Failure    -> return `Failure
  | `Success v' -> return' (f v')

let string_of_chars s =
  s
  |> List.map (String.make 1)
  |> String.concat ""

let int_of_char_lst lst =
  try return (`Success (int_of_string (string_of_chars lst)))
  with Failure "int_of_string" -> return `Failure

let int_of_char_lst' lst =
  try return (`Success (Int64.of_string (string_of_chars lst)))
  with Failure "int_of_string" -> return `Failure

let find_sublist_char (r : Reader.t) (c : char) : char list can_fail Deferred.t =
  let rec helper (acc : char list) : char list can_fail Deferred.t =
    Reader.read_char r >>= fun read_result ->
    match read_result with
    | `Ok h when h = c -> return' acc
    | `Ok h            -> helper (h::acc)
    | `Eof             -> return `Failure in
  helper [] >>==| fun lst ->
  List.rev lst

let find_sublist_index (r : Reader.t) (index : int)  : char list can_fail Deferred.t =
  let rec helper (acc : char list) (i : int) : char list can_fail Deferred.t =
    if i > 0 then
      Reader.read_char r  >>= fun read_result ->
      match read_result with
      | `Ok h -> helper (h::acc) (i-1)
      | `Eof  -> return `Failure
    else if (i < 0) then
      return `Failure
    else
      return' acc in
  helper [] index >>==| fun lst ->
  List.rev lst

let rec parse_int (r : Reader.t)  : t can_fail Deferred.t =
  find_sublist_char r 'e'  >>=== fun lst ->
  int_of_char_lst' lst >>==| fun i ->
  BInt i

and parse_string (r : Reader.t) (c : char)  : t can_fail Deferred.t =
  find_sublist_char r ':'  >>=== fun lst ->
  int_of_char_lst (c :: lst) >>=== fun i ->
  find_sublist_index r i  >>==| fun char_lst ->
  BString (string_of_chars char_lst)

and parse_list (r : Reader.t)  : t can_fail Deferred.t =
    let rec lst_helper acc =
      Reader.read_char r  >>= fun read_result ->
      match read_result with
      | `Ok 'e' -> return' acc
      | `Ok x   -> decode_h (Some x) r  >>=== fun b ->
                   lst_helper (b :: acc)
      | `Eof    -> return `Failure in
  lst_helper [] >>==| fun b ->
  BList (List.rev b)

and parse_dict (r : Reader.t) : t can_fail Deferred.t =
  let extract_string b =
    match b with
    | BString s -> return' s
    | _         -> return `Failure in
  let rec dict_helper acc =
    Reader.read_char r  >>= fun read_result ->
    match read_result with
    | `Ok 'e' -> return' acc
    | `Ok x   -> parse_string r x >>=== fun b1 ->
                 extract_string b1 >>=== fun s ->
                 decode_h None r >>=== fun b2 ->
                 dict_helper ((s, b2) :: acc)
    | `Eof    -> return `Failure in
  dict_helper [] >>==| fun b ->
  BDict (List.rev b)

and decode_h (c : char option) (r : Reader.t) : t can_fail Deferred.t =
  let interpret c =
    match c with
    | `Ok 'i' -> parse_int r
    | `Ok 'd' -> parse_dict r
    | `Ok 'l' -> parse_list r
    | `Ok x   -> parse_string r x
    | `Eof    -> return `Failure in
  match c with
  | Some c -> interpret (`Ok c)
  | None   -> Reader.read_char r  >>= fun read_result ->
              interpret read_result

let decode (r : Reader.t) : t can_fail Deferred.t =
  decode_h None r >>=== fun t ->
  return' t

let encode_string s =
  string_of_int (String.length s) ^ ":" ^ s

let rec encode_blst lst =
  List.fold_left (fun acc x -> acc ^ (encode x) ) "" lst

and encode_bdict lst =
  let compare_dict (s1, _) (s2, _) =
    compare s1 s2 in
  List.sort compare_dict lst
  |> List.fold_left (fun acc (k, v) -> acc ^ (encode_string k) ^ (encode v)) ""

and encode (b : t) : string =
  match b with
  | BInt i     -> "i" ^ (Int64.to_string i) ^ "e"
  | BString s  -> encode_string s
  | BList lst  -> "l" ^ (encode_blst lst) ^ "e"
  | BDict dict -> "d" ^ (encode_bdict dict) ^ "e"