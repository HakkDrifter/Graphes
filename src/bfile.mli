(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> string graph