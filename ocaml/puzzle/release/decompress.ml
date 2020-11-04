open Huffman

let rec bits_to_int l = match l with
  | [] -> 0
  | Zero::tl -> 2*bits_to_int tl
  | One::tl  -> 1 + 2*bits_to_int tl

let bit_of_int i =
  if i = 0 then Zero 
  else if i = 1 then One
  else failwith "invalid bit"

let bitlist_of_intlist il = 
  List.rev(List.rev_map bit_of_int il)

let chars_of_bitlist c bitlist =
  List.rev(List.rev_map (fun _ -> c) bitlist)

(* Returns bitstream with the first n bits removed and converted back
 * to an integer *)
let read_n_bits_to_int (n : int) (bitstream : int list) : int * int list =
  let rec pop_n i word lst =
    if i = n then (word, lst)
    else match lst with
        h :: t -> pop_n (i + 1) ((word lsl 1) lor h) t
      | [] -> failwith "not enough bits remaining"
  in pop_n 0 0 bitstream

let to_bits (n : int) (bitstream : int list) (num : int) : int list =
  let rec each_bit accum i =
    if i < n then
      each_bit (((num lsr i) land 1) :: accum) (i + 1)
    else accum in
  each_bit bitstream 0

let write_chars (f : out_channel) (chrs : char list) : unit =
  List.iter (output_char f) chrs

let take n xs =
  let rec take' n xs acc = 
    match n, xs with
    | 0, xs -> acc
    | n, [] -> failwith "take"
    | n, x::xs -> take' (n-1) xs (x::acc)
  in 
  List.rev (take' n xs []);;

(* Returns the contents of f as a bitstream *)
let read_bits (f : in_channel) : int list =
  (* reversed list of bytes *)
  let rec read_bytes bytes =
    let res = try Some (input_byte f)
      with End_of_file -> None in
    match res with
      None -> bytes
    | Some(b) -> read_bytes (b :: bytes) in
  List.fold_left (to_bits 8) [] (read_bytes [])

let string_of_bit = function
  | Zero -> "0"
  | One -> "1"

let marshal_from_file ifname =
  let ic = open_in_bin ifname in
  let (tree: 'a encoding) = Marshal.from_channel ic in
  let (padding_size: int) = Marshal.from_channel ic in
  let bitlist = bitlist_of_intlist (read_bits ic) in
  let bitlist_size = (List.length bitlist) - padding_size in
  let bitlist = take bitlist_size bitlist in
  close_in ic;
  tree, bitlist

let cut suffix s =
  if String.length s < String.length suffix
  then failwith "Error: filename must end with "^suffix
  else let suffstart = String.length s - String.length suffix in
       if String.sub s suffstart (String.length suffix) <> suffix
       then failwith "Error: filename must end with "^suffix
       else String.sub s 0 suffstart

let () = 
  if Array.length Sys.argv < 2 then
    print_endline "usage: decompress [filename]"
  else
    let ifname = Sys.argv.(1) in
    let oc = stdout in  
    match marshal_from_file ifname with
    | Empty, _              -> ()
    | Tree(Leaf x), bitlist -> write_chars oc (chars_of_bitlist x bitlist)
    | Tree(tree)  , bitlist -> write_chars oc (decode tree bitlist);
    close_out oc
