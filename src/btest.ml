open Tools
open Fulk
open BaseballCase
open Bfile

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile team \n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and team = Sys.argv.(2)
  in

(*

  (* Open file *)
  let table = from_file infile in 

*)

  ()