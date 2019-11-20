open Graph

let rec findChain gr src sink acu marked = 

    let rec aux out_arcs gr1 src1 sink1 acu1 marked1 = 
        match out_arcs with
        | [] -> []
        | x::rest -> if List.exists (fun a -> fst x == a) marked1 then aux rest gr1 src1 sink1 acu1 (src1::marked1) else let res = findChain gr1 src1 sink1 acu1 (src1::marked1) in if res == [] then aux rest gr1 src1 sink1 acu1 (src1::marked1) else res

    in

    
    let a = out_arcs gr src in
        match a with
        | [] -> []
        | x::rest -> if fst x == sink then (src,fst x,snd x)::acu else aux a gr (fst x) sink ((src,fst x,snd x)::acu) (src::marked)


