(** Module for performing Huffman coding.
  
    This interface is agnostic about the type of characters that are used.  The
    most typical use is to build [char encoding]s from [char list]s, but other
    types are possible. *)

type bit = Zero | One

type 'a hufftree = Leaf of 'a | Node of 'a hufftree * 'a hufftree
type 'a encoding = Empty | Tree of 'a hufftree

(** given a list of characters, build an optimal Huffman tree for encoding that
    list.  If the input list is empty, then [Empty] should be returned; if the
    input list contains only repetitions of a single character [c], then
    [Singleton c] should be returned.  *)
val build_tree : 'a list -> 'a encoding

(** [encode tree chars] produces the bit list representing [chars] using the
    Huffman tree [tree].

    The characters in [chars] must all be represented in [tree];
    if passed a malformed input, [encode] should raise an exception.  *)
val encode : 'a hufftree ->  'a list -> bit list

(** [decode tree bits] decodes [bits] using the encoding scheme specified for
    the [encode] function.
  
    It should raise an exception if [bits] is not valid according to [tree]. *)
val decode : 'a hufftree -> bit list ->  'a list

