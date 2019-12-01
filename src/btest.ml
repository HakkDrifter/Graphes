open Gfile
open Tools
open Fulk
open BaseballCase

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

   

  (*let res = findFlow( fst (findChain (gmap graph int_of_string) _source _sink [] [] )) in Printf.printf"flow : %d ||||| %!" res *)

  let res = find_chain (gmap graph int_of_string) _source _sink [] []  in List.iter (fun (x,y,z) -> (Printf.printf"res : %d -> %d %!" x y)) (List.rev (fst res)) 
  ; let flow = find_flow( fst res) in Printf.printf" \n flow -> %d %!" flow ; 

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile (gmap(fulk (gmap graph int_of_string) _source _sink)(string_of_int)) (*; export(outfile)(graph) *) in

  ()