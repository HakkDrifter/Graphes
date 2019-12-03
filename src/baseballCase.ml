open Graph
open Fulk

type team = (id * int  * int * int)

type game = (id  * id * int * int)

type table = (team list * game list) 

exception Table_error of string

let cantor_pairing (a,b) = 
let sort (x,y) = if x < y then (x,y) else (y,x) in 
let vals = sort (a,b) in
(fst vals + snd vals)*(fst vals + snd vals + 1)/2 + snd vals 

let rec get_team tb tid = match fst tb with 
| [] -> raise (Table_error ("team " ^ string_of_int tid ^ " does not exist in this table."))
| (id,w,l,gr)::rest -> if tid == id then (id,w,l,gr) else get_team (rest, snd tb) tid 

let rec add_teams teams gr = match teams with 
| [] -> gr
| (id,_,_,_)::rest -> add_teams rest (new_node gr id)

let rec add_games games gr = match games with 
| [] -> gr
| (_,_,id,_)::rest -> add_games rest (new_node gr id)

let build_nodes table gr = (add_teams (fst table) (add_games (snd table) (new_node (new_node gr 0) (-1)) ) )

let rec build_game_arcs games gr = match games with
| [] -> gr 
| (t1,t2,gid,gamesRem)::rest -> build_game_arcs rest (new_arc (new_arc (new_arc gr gid t2 max_int) gid t1 max_int) 0 gid gamesRem)

let rec build_team_arcs teams gr team = match (teams,team) with
| ([],_) -> gr 
| ((id,w,l,gamesRem)::rest,(_,teamW,teamL,teamgamesRem)) -> build_team_arcs rest (new_arc gr id (-1) (teamW + teamgamesRem - w)) team

let build_arcs table gr team = build_game_arcs (snd table) (build_team_arcs (fst table) gr team) 

let build_graph table teamid = build_nodes table (build_arcs table empty_graph (get_team table teamid))

let check_state gr = true 

let empty_table = ([],[])

let new_team tb id w l gr = ((id,w,l,gr):: fst tb, snd tb)

let rec game_exist tb id = match tb with 
| (_,[]) -> false 
| (teams, (_,_,gid,_)::rest) -> gid == id || game_exist (teams,rest) id 

let new_game tb t1 t2 gr = let gid = cantor_pairing (t1,t2) in if game_exist tb gid then tb else (fst tb,(t1,t2,gid,gr) :: snd tb)


