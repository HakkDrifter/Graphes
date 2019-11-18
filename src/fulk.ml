open Graph

let rec aux arcs sink acup acug =
    match arcs with 
    | [] -> []::acug  
    | x::rest -> if fst x = sink then acup::acug else 
        match (snd x) with
            | (a,b) -> if a < b then List.append (aux rest sink (x::acup) acug) acug else acug  

let find_chains gr src sink = aux (out_arcs gr src) sink [] [] 
