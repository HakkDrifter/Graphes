open Tools
open Fulk
open BaseballCase
open Bfile
open Graph 
open Tools

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile teamID \n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and team = Sys.argv.(2)
  in

  let teamId = (int_of_string team) in 

  (* Open file *)
  let table = from_file infile teamId in 

  (*print_table table ; *)

  let graph = build_graph table in 

  (*print_graph graph ; *)

  if check_state graph table then Printf.printf "\n team %s is not eliminated %!" team else Printf.printf "\n team %s is eliminated %!" team ; 

  ()