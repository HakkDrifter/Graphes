open Gfile
open Tools
open Fulk

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in 

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile (gmap(add_arc((gmap(graph))(int_of_string))(1)(3)(10))(Int.to_string)) (*; export(outfile)(graph) *) in

  let res = [(2,5,3);(5,8,9)](*findChain (gmap graph int_of_string) _source _sink [] []*) in
   List.iter (fun (x,y,z) -> Printf.printf" %d %!"x) (res) ;

  ()

