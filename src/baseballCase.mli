
open Graph

(*
team : id wins losses remainingGames 
*)
type team = (id * int  * int * int)

(*
game : team1ID team2ID gameID remainingGames
*)
type game = (id  * id * int * int)

type table = (team list * game list)


exception Table_error of string

(*
used to have unique game id 
*)
val cantor_pairing : (int*int) -> int 

val new_team : table -> id -> int -> int -> int -> table 

(*val team_exist : table -> int -> bool *)

val get_team : table -> id -> team 

val new_game : table -> id -> id -> int -> table 

val game_exist : table -> int -> bool 

val empty_table : table 

val build_graph : table -> id -> int graph 

val check_state : int graph -> bool ;; 