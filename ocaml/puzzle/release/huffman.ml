#load "str.cma";;

type 'a hufftree = Leaf of 'a | Node of 'a hufftree * 'a hufftree
type 'a encoding = Empty | Tree of 'a hufftree
type bit = Zero | One


let build_tree chars = failwith "make me a sandwich"

let encode enc chars = failwith "what? make it yourself."

let decode enc bits  = failwith "sudo make me a sandwich"

