data In: a -> List a -> Type where
  Here : In x (x::xs)
  There : (later: In x xs)-> In x (y::xs)

data Node = N Nat

Graph: Type
Graph = List (Node, (List Node))

data Contains: Node -> Graph -> Type where
  ThisOne : Contains x ((x,_)::xs)
  OtherOne : (later: Contains x xs)-> Contains x ((y,_)::xs)

implementation Eq Node where
   (N a) == (N b) = a == b
   (N a) /= (N b) = a /= b


insertConnection: Node -> Node -> Graph -> Graph
insertConnection n sn [] = []
insertConnection n sn ((gn, cons) :: xs) = if sn == gn
                            then (sn, n::cons)::xs
                            else (gn, cons)::(insertConnection n sn xs)

insert: Node -> List Node -> Graph -> Graph
insert node cns graph = (node, cns)::(insertCns node cns graph) where
                        insertCns: Node -> List Node -> Graph -> Graph
                        insertCns node [] graph = graph
                        insertCns node (cn :: cns) graph = insertCns node cns (insertConnection node cn graph)

getConnected: Node -> Graph -> List Node
getConnected cn [] = []
getConnected cn ((n, cns) :: ys) = if cn == n
                            then cns
                            else getConnected cn ys

depthSearch: Node -> Graph -> List Node
depthSearch sn graph = depthSearch' [sn] [] graph where
                depthSearch':List Node -> List Node -> Graph -> List Node
                depthSearch' [] _ _ = []
                depthSearch' (n::nn) vns graph = if elem n vns
                            then depthSearch' nn vns graph
                            else n::depthSearch' ((getConnected n graph)++nn) (n::vns) graph

connectInDFS:
  (elem : Node) ->
  (g : Graph) ->
  (connectTo : List Node) ->
  (inGraph : (n : Node) -> In n connectTo -> Contains n g) ->
  (n : Node) ->
  In n connectTo ->
  In n (depthSearch elem (insert elem connectTo g)) --added connectTo

graph: Graph
graph = insert (N 5) [N 4] (insert (N 4) [(N 2), (N 3)](insert (N 3) [(N 1)](insert (N 2) [N 1] (insert (N 1) [] []))))
