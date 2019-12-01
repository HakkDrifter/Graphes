
open Graph
open Tools

type 'a arc = (id * id *'a)

type 'a chain_res = (('a arc) list  * id list)

let rec chain_iterator arcs gr src(*used only to form arc if sink found*) sink acu marked = 

let aux grA srcA sinkA acuA markedA = 
    let arcs = out_arcs grA srcA in
    match arcs with 
    | [] -> ([],markedA)
    | x::rest -> chain_iterator arcs grA srcA sinkA acuA markedA
in 

    match arcs with 
    | [] -> ([],marked) (*when all arcs have been iterated *)
    | x::rest -> if (fst x) == sink then ((src,fst x,snd x)::acu,[]) else (*Found -> return result*)
    if List.exists (fun a -> fst x == a) marked then chain_iterator rest gr src sink acu ((fst x)::marked) else (*Iterate on next arc with updated marked*)
    let res = aux gr (fst x) sink ((src,fst x,snd x)::acu) ((fst x)::marked) in  if (fst res) == [] then chain_iterator rest gr src sink acu ((fst x)::marked) (*No result deeper ; Iterate on next arc with updated marked*)
    else res (* Forward result*)


 
let find_chain gr src sink acu marked = 
    if src == sink then ([],[])
    else chain_iterator (out_arcs gr src) gr src sink acu (src::marked)

let rec find_flow path =
    match path with
    | [] -> max_int
    | x::rest -> (match x with
                | (a,b,c) ->  min c (find_flow rest) ) 


let rec fulk gr src sink = 

    let rec add_chain gr1 ch n =
        (match ch with
        | [] -> gr1
        | (a,b,_)::rest -> add_chain (add_arc (add_arc gr1 a b (-n)) b a n) rest n )

    in
        let chain = find_chain gr src sink [] [] in
            match (find_flow (fst chain)) with
            | a -> if a == max_int || a == 0 then gr else fulk (add_chain gr (fst chain) a) src sink           


let rec fulk_debug gr src sink = 

    let rec add_chain gr1 ch n =
        (match ch with
        | [] -> gr1
        | (a,b,_)::rest -> (* Printf.printf" \n Value -> %d %!" n ; *) add_chain (add_arc (add_arc gr1 a b (-n)) b a n) rest n )

    in
        Printf.printf"\n --- \n%!" ; 
        let chain = find_chain gr src sink [] [] in Printf.printf"\n %!" ; List.iter (fun (x,y,z) ->(Printf.printf"chain : %d -> %d %!" x y)) (List.rev (fst chain));
            match (find_flow (fst chain)) with
            | a ->  Printf.printf"\n flow -> %d %!" a ; Printf.printf"\n %!"; e_iter gr (fun a b v -> Printf.printf" | arc: %d -> %d value: %d %!" a b v);
            if a == max_int || a == 0 then gr else fulk (add_chain gr (fst chain) a) src sink    