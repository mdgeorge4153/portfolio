open TestHarness
open Helpers

module Student = Huffman
module Solution = Huffman_sol 

let needed_chars (a : int array) : int list =
  let tup_list = Array.to_list (Array.mapi (fun index count -> (index, count)) a) in 
  List.map (fun (index, count) -> index) (List.filter (fun (index, count) -> count > 0) tup_list)

(** OCaml batteries-included code: https://github.com/ocaml-batteries-team/batteries-included **)
let string_filter f s =
  let len = String.length s in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      let c = String.unsafe_get s i in
  if f c then Buffer.add_char sc c
    done;
  Buffer.contents sc

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

(* true if whether lst2 contains every element of lst1 *)
let rec contains (lst1: 'a list) (lst2: 'a list) : bool =
  match lst1 with
    [] -> true
  | h::t -> List.mem h lst2 && (contains t lst2)
(** End batteries-included **)

let charlist_to_intlist (lst: char list) : int list =
  List.map (fun x -> Char.code x) lst

let shortened_string_of_int_list (lst: int list) : string =
  let rec helper l accum =
    if List.length accum > 50 then accum
    else
      match l with
        h::t -> helper t (h::accum)
      | [] -> accum
  in
    let s = string_of_int_list (List.rev (helper lst []))
    in if List.length lst > 50 then s ^ " ... <output trimmed>" else s
 
let helper (text: char list) (student_tree: Student.huffmantree)
(student_encoded_list: int list) (tree_name: string) (text_name: string) =
  let student_codes = ref [] in 
  let student_decoded = ref [] in
  assert_no_throw ("get_codes("^tree_name^")") (lazy (student_codes := (Student.get_codes student_tree)));

  let freqs = Array.make 256 0 in Solution.countchars text freqs;

  let student_chars = List.sort compare (List.map (fun (char, _) -> Char.code
    char) !student_codes) in
  (* code must pass one of the next two cases *)
  (* TODO: change this so it actually checks all 256 chars *)
  let special_checker a _ =
    a = 256 || a = List.length (needed_chars freqs) in
  assert_equal ("get_codes("^tree_name^") yields either ALL characters or nonzero frequency characters") 
    (lazy (List.length student_chars)) (lazy (-1))
    (string_of_int) (special_checker) (); 

  assert_equal ("List.length (decode (encode " ^ text_name ^ ")) = List.length " ^ text_name)
    (lazy (student_decoded := (Student.decode student_tree student_encoded_list); List.length !student_decoded))
    (lazy (List.length text))
    (string_of_int) (=) ();

  assert_equal "decode (encode text)) = text"
    (lazy (charlist_to_intlist !student_decoded))
    (lazy (charlist_to_intlist text))
    (shortened_string_of_int_list) (=) ();

  ()

let run_tests (filepath: string) : unit =
  print_endline ("\nStarting tests on " ^ filepath);

  (* Get rid of all newlines in the input text. *)
  let text = explode (string_filter (fun c -> c <> '\n') (implode (Util.load_chars filepath))) in
  (* Build a dummy huffman tree, just so we can have a ref cell with a valid
   * type. Hopefully this doesn't timeout... *)
  let student_tree_encodedlist = ref (Student.encode ['a']) in
  (* Bitwise representation of huffman tree *)
  let student_huffman_bits = ref [] in
  let student_tree_regrown = ref (fst (!student_tree_encodedlist)) in
  assert_no_throw ("encode(" ^ filepath ^")") (lazy (student_tree_encodedlist := (Student.encode text)));

  (* Do most tests *)
  helper text (fst !student_tree_encodedlist) (snd !student_tree_encodedlist) "student_tree" filepath;

  (* Test prepend/regrow. *)
  assert_no_throw "prepend_tree student_tree []" (lazy (student_huffman_bits := (Student.prepend_tree (fst !student_tree_encodedlist) [])));
  assert_no_throw "regrow_tree student_huffman_bits" (lazy (
    let (new_tree, lst) = Student.regrow_tree !student_huffman_bits in
    student_tree_regrown := new_tree
  ));

  print_endline "-- running tests again with regrown tree --";

  (* Repeat most tests, with the regrown tree. *)
  helper text !student_tree_regrown (snd !student_tree_encodedlist) "regrown_student_tree" filepath;
  ()

let run () : unit = 

  run_tests "tests/huffman/empty.txt";
  run_tests "tests/huffman/random1.txt";
  run_tests "tests/huffman/all-ascii.txt";

(* for reference, here are descriptions of the test functions in TestHarness that you can use:
 *
 * let assert_equal code lazy_student_answer lazy_solution_answer string_of_answer equal
 * ?timeout ?exceptions_equal ()
 *
 * Checks that lazy_student_answer = lazy_solution_answer
 * (i.e. assert_equal "fact (fact 3)" (lazy (Part1.fact (Part1.fact 3))) (lazy 720) string_of_int (=) ()
 *
 * let test_fun
 *    ~fun_name
 *    ~student_fun
 *    ~solution_fun
 *    ?(equal=(=))
 *    ~string_of_input
 *    ~string_of_answer
 *    ~inputs
 *    ?(timeout=defaultTimeout) ()
 * This is exactly like the test structure in the old test harness.  You supply it with a student's function and the
 * solution's implementation of the same function.  Then you give it all of the inputs you'd like to test, and it
 * compares the two functions for each input.
 *
 * let test_fun_different_inputs
 *    ~fun_name
 *    ~student_fun
 *    ~solution_fun
 *    ?(equal=(=))
 *    ~string_of_input
 *    ~string_of_answer
 *    ~student_inputs
 *    ~solution_inputs
 *    ?(timeout=defaultTimeout)
 *    ?(exceptions_equal=(fun _ _ -> true)) ()
 * Same as test_fun except with different inputs for the student and solution functions
 *
 * Note: Some of the test functions (like assert_equal) require expressions to be lazily evaluated.
 * To write a lazy expression, write "lazy (expression)".  Also, a parameter prefixed by a ? is optional *)

