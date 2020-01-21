data Color = Black | Red

-- succColor: Color -> Color
-- succColor Black = Red
-- succColor Red = Black
data RBNode = N Color RBNode Int RBNode | Empty
data NodeRel = NL RBNode RBNode RBNode RBNode

balance: RBNode -> RBNode

insertBW: (RBNode, NodeRel) -> (RBNode, NodeRel)


insert: Int -> RBNode -> RBNode
insert nv t@(N c l v r) = let inserted = case (compare nv v) of
                                          LT => insert nv l
                                          EQ => t
                                          GT => insert nv r
                                        in balance inserted

-- insert nv Empty = N Red Empty nv Empty


-- data RBTree : Type -> Color -> Type where
--   Empty : (Ord elem) =>  (c: Color) -> RBTree elem c
--   Node : (Ord elem) => (c: Color) ->
--               RBTree elem (succColor c) ->
--               elem ->
--               RBTree elem (succColor c) ->
--               RBTree elem (succColor c)
--
-- insert: elem -> RBTree elem c1 -> RBTree elem c2
-- insert x (Empty c1) = Node c1 (Empty (succColor c1)) x (Empty (succColor c1))
-- insert x (Node c y z w) = ?insert_rhs_2
--
-- test: RBTree Int Black
-- test = Node Red (Empty Black) 1 (Empty Black)

-- insert: elem -> (RBTree elem _) -> (RBTree elem _)
-- insert val (Empty _) = Node Red (Empty _) val (Empty _)
-- insert val (Node c x y z) = ?insert_rhs_2


-- insert: elem ->  RBTree Int Black -> RBTreee Int Black

-- test: RBTree color
-- test = Node Red (Empty Black) 1 (Empty Black)
