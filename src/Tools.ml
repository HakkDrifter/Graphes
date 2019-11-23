open Graph

let clone_nodes (gr : 'a graph) = n_fold(gr)(new_node)(empty_graph);;

let gmap (gr :'a graph) (f : ('a -> 'b)) = e_fold (gr) (fun g id1 id2 a-> new_arc g id1 id2 (f a)) (clone_nodes(gr));;

let add_arc g id1 id2 n = 
    let res = find_arc(g)(id1)(id2) in if res = None then new_arc(g)(id1)(id2)(n) else let newValue = Option.get(res) + n in if newValue < 0 then new_arc(g)(id1)(id2)(0) else new_arc(g)(id1)(id2)(newValue);;

