open Graph

type 'a arc = (id * id *'a)

val findChain: int graph -> id -> id -> (int arc) list -> id list -> (int arc) list

val findFlow: (int arc) list -> int 

val fulk: int graph -> id -> id -> int graph ;;