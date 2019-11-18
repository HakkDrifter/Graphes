open Graph

type flowLabel = (int * int)

val findChains: flowLabel graph -> id -> id -> ((flowLabel out_arcs) list) list

val findBestChain: ((flowLabel out_arcs) list) list -> (flowLabel out_arcs) list

val findFlow: (flowLabel out_arcs) list -> int 

val fulk: flowLabel graph -> id -> id -> flowLabel graph ;;