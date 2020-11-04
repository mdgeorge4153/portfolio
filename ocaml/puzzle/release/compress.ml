open Huffman

let rec int_to_bits n =
  if n = 0 then []
  else if n mod 2 = 0 then Zero::int_to_bits (n/2)
  else One::int_to_bits (n/2)

let int_of_bit b = 
  match b with
  | Zero -> 0
  | One -> 1

let intlist_of_bitlist bl = 
  List.rev (List.rev_map (int_of_bit) bl)

let bitlist_of_chars chars = 
  List.rev (List.rev_map (fun _ -> Zero) chars)

(* Write a stream of bits to a byte-oriented out channel.*)
(* Pad last byte to length 8 with the supplied padding. *)
let write_bits (f : out_channel) (bits : int list) (padding : int list) : unit =
  assert (List.length padding >= 8); (* sanity check *)
  let next_byte bits =
    let rec nb n byte bits =
      if n = 8 then (byte, bits)
      else match bits with
        | [] -> (fst (nb n byte padding), [])
        | h :: t -> nb (n + 1) (byte lsl 1 lor h) t
    in nb 0 0 bits in
  let rec write_char bits =
    if bits = [] then () else
    let (byte, bits) = next_byte bits in
    output_byte f byte; write_char bits in
  write_char bits;
  close_out f

(* Returns a char list of the contents of the file at fname *)
let load_chars (fname: string) : char list = 
  let file = open_in fname in
  let stringlist =
    try
      let rec read_lines acc =
        let next = try Some (input_char file) with End_of_file -> None in
        match next with Some s -> read_lines (s :: acc) | None -> acc in
      read_lines []
    with exc -> close_in file; raise exc in
  List.rev  stringlist

let string_of_bit = function
  | Zero -> "0"
  | One -> "1"


let take n xs =
  let rec take' n xs acc = 
    match n, xs with
    | 0, xs -> acc
    | n, [] -> failwith "take"
    | n, x::xs -> take' (n-1) xs (x::acc)
  in 
  List.rev (take' n xs []);;

let marshal_to_file filename enc bitlist =
  let oc = stdout in
  let padding_size = ((8 - ((List.length bitlist) mod 8)) mod 8) in
  Marshal.to_channel oc enc [];
  Marshal.to_channel oc padding_size [];
  write_bits oc (intlist_of_bitlist bitlist) [0;0;0;0;0;0;0;0];
  close_out oc

let () =
  if Array.length Sys.argv < 2 then
    print_endline "usage: compress [filename]"
  else
    let ifname  = Sys.argv.(1) in
    let ofname  = ifname ^ ".huff" in
    let chars   = load_chars ifname in
    match build_tree chars with
    | Empty        as enc -> marshal_to_file ofname enc []
    | Tree(Leaf _) as enc -> marshal_to_file ofname enc (bitlist_of_chars chars)
    | Tree(tree)   as enc -> marshal_to_file ofname enc (encode tree chars)
