open Graph
open Tools

type 'a arc = (id * id *'a)

let rec findChain gr src sink acu marked = 
    let rec aux out_arcs gr1 src1 sink1 acu1 marked1 = Printf.printf" %d %!" 11 ;
        match out_arcs with
        | [] -> []
        | x::rest -> if List.exists (fun a -> fst x == a) marked1 then aux rest gr1 src1 sink1 acu1 (src1::marked1) else let res = findChain gr1 src1 sink1 acu1 (src1::marked1) in if res == [] then aux rest gr1 src1 sink1 acu1 (src1::marked1) else res

    in

    
    let a = out_arcs gr src in
        match a with
        | [] -> []
        | x::rest -> if fst x == sink then (src,fst x,snd x)::acu  else aux a gr (fst x) sink ((src,fst x,snd x)::acu) (src::marked)


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
            match (findFlow chain) with
            | a -> if a == max_int then gr else fulk (add_chain gr chain a) src sink           


(*Printf.printf" %d %!" a ; *) 