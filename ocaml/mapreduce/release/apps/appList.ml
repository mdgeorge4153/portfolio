open WordCount
open InvertedIndex
open DnaSequencing
open RelationComposition
open CommonFriends
(* open MatrixMultiply *)

let list_apps () =
  let apps = String.concat "\n  - " (MapReduce.list_apps ()) in
  if apps = "" then "No apps installed!" else "Installed apps:\n\n  - "^apps

