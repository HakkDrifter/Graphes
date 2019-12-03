
open Graph
open BaseballCase

type path = string

(* Values are read as strings. *)
val from_file: path -> table

(* Read a graph from a file,
 * Write a graph to a file. *)