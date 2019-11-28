open Graph

type 'a arc = (id * id *'a)

type 'a chainRes = (('a arc) list  * id list)

val findChain: int graph -> id -> id -> (int arc) list -> id list -> int chainRes

val findFlow: (int arc) list -> int 

val fulk: int graph -> id -> id -> int graph ;;