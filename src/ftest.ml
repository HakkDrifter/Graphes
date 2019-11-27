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

   (*List.iter (fun (x,y,z) -> Printf.printf" %d %!"x) (res) ; *)

  let res = findFlow( findChain (gmap graph int_of_string) _source _sink [] [] ) in Printf.printf" %d %!" res ;

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile (gmap(fulk (gmap graph int_of_string) _source _sink)(Int.to_string)) (*; export(outfile)(graph) *) in

  ()

