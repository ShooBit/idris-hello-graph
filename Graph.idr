data In: a -> List a -> Type where
  Here : In x (x::xs)
  There : (later: In x xs)-> In x (y::xs)

data Node = N Nat

implementation Eq Node where
   (N a) == (N b) = a == b
   (N a) /= (N b) = a /= b

Graph: Type
Graph = List (Node, (List Node))

data Contains: Node -> Graph -> Type where
  ThisOne : Contains x ((x,_)::xs)
  OtherOne : (later: Contains x xs)-> Contains x ((y,_)::xs)




insertConnection: Node -> (n : Node) -> (g: Graph) -> (Contains n g) -> Maybe Graph
insertConnection x n ((n, b) :: xs) ThisOne = ?insertConnection_rhs_1
insertConnection x n ((y, b) :: xs) (OtherOne later) = ?insertConnection_rhs_2

-- insertConnection n sn [] (OtherOne []) = Nothing
-- insertConnection n sn ((gn, cons) :: xs) = if sn == gn
--                             then (sn, n::cons)::xs
--                             else (gn, cons)::(insertConnection n sn xs)

-- insert: Node -> List Node -> Graph -> Graph
-- insert node cns graph = (node, cns)::(insertCns node cns graph) where
--                         insertCns: Node -> List Node -> Graph -> Graph
--                         insertCns node [] graph = graph
--                         insertCns node (cn :: cns) graph = insertCns node cns (insertConnection node cn graph)
--
getConnected: Node -> Graph -> List Node
getConnected cn [] = []
getConnected cn ((n, cns) :: ys) = if cn == n
                            then cns
                            else getConnected cn ys

depthSearch: Node -> Graph -> List Node
depthSearch sn graph = depthSearch' [sn] [] graph where
                depthSearch':List Node -> (visited: List Node) -> Graph -> List Node
                depthSearch' [] _ _ = []
                depthSearch' (n::nn) vns graph = if elem n vns
                            then depthSearch' nn vns graph
                            else n::depthSearch' ((getConnected n graph)++nn) (n::vns) graph

get: (n: Node) -> (g: Graph) -> {auto prf : Contains n g} -> (Node, List Node)
get n ((n, b) :: xs) {prf = ThisOne} = (n,b)
get n ((y, b) :: xs) {prf = OtherOne later} = get n xs

-- connectInDFS:
--   (elem : Node) ->
--   (g : Graph) ->
--   (connectTo : List Node) ->
--   (inGraph : (n : Node) -> In n connectTo -> Contains n g) -> --added Contains, rewrote In
--   (n : Node) ->
--   In n connectTo ->
--   In n (depthSearch elem (insert elem connectTo g)) --added connectTo
-- -- connectInDFS elem g connectTo inGraph n x = ?connectInDFS_rhs
--
-- graph: Graph
-- graph = insert (N 5) [N 4] (insert (N 4) [(N 2), (N 3)](insert (N 3) [(N 1)](insert (N 2) [N 1] (insert (N 1) [] []))))
