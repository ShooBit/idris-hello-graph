-- import Data.Vect
-- data In: a -> List a -> Type where
--   Here: (x: a) -> (xs: List a) -> In x (x::xs)
--   -- Three: (x,y : a) -> (xs: List a) -> In x xs -> In x (y:xs)
--
data Node = N Nat

implementation Eq Node where
   (N a) == (N b) = a == b
   (N a) /= (N b) = a /= b
--
Graph: Type
Graph = List (Node, (List Node))

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

graph: Graph
graph = insert (N 5) [N 4] (insert (N 4) [(N 2), (N 3)](insert (N 3) [(N 1)](insert (N 2) [N 1] (insert (N 1) [] []))))
