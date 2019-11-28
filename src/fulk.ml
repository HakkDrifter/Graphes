open Graph
open Tools

type 'a arc = (id * id *'a)

type 'a chainRes = (('a arc) list  * id list)

let rec findChain gr src sink acu marked = 
    let rec aux out_arcs1 gr1 src1 sink1 acu1 marked1 = 
        match out_arcs1 with
        | [] -> ([],src::marked)
        | x::rest -> Printf.printf"aux : %d -> %d \n %!" src1 (fst x) ; if (fst x) == sink1 then ((src1,fst x,snd x)::acu,src1::marked) else if List.exists (fun a -> fst x == a) marked1 then aux rest gr1 (fst x) sink1 acu1 (src1::marked1) else let res = findChain gr1 src1 sink1 acu1 (src1::marked1) in if (fst res) == [] then let newMarked = List.append(snd res) (src1::marked1) in (aux rest gr1 src1 sink1 acu1 newMarked)  else res

    in

    
    let a = out_arcs gr src in List.iter (fun (x,y) -> Printf.printf"outarcs : %d \n %!"x) (a) ; 
        match a with 
        | [] -> ([],src::marked)
        | x::rest -> Printf.printf"chain : %d -> %d \n %!" src (fst x); if fst x == sink then ((src,fst x,snd x)::acu,src::marked)  else aux a gr (fst x) sink ((src,fst x,snd x)::acu) (src::marked)


let rec findFlow path =
    match path with
    | [] -> max_int
    | x::rest -> (match x with
                | (a,b,c) -> if c <= 0 then findFlow rest else min c (findFlow rest) ) 


(*Find chain ne marche pas pour noeud lié par un arc *)

let rec fulk gr src sink = 

    let rec add_chain gr1 ch n =
        (match ch with
        | [] -> gr1
        | (a,b,_)::rest -> add_chain (add_arc (add_arc gr1 a b (Int.neg n)) b a n) rest n  )

    in

        let chain = findChain gr src sink [] [] in 
            match (findFlow (fst chain)) with
            | a -> if a == max_int then gr else fulk (add_chain gr (fst chain) a) src sink           


(*Printf.printf" %d %!" a ; *) 