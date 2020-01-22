data Color = Black | Red
data RBNode = N Color Int
data RBTree = TN RBTree RBNode RBTree | Empty
data Cxt = T | L RBTree RBNode Cxt| R Cxt RBNode RBTree

Loc: Type
Loc = (RBTree, Cxt)

left: Loc -> Loc
left (TN l n r, c) = (l, L r n c)

right: Loc -> Loc
right (TN l n r, c) = (r, R c n l)

up: Loc -> Loc
up (l, L r n c) = (TN l n r, c)
up (r, R c n l) = (TN l n r, c)

top: RBTree -> Loc
top t = (t,T)

extract: Loc -> RBTree
extract (t, T) = t
extract l = extract (up l)

nav_ins: Int -> Loc -> Loc
nav_ins newVal (Empty, ctx) = (TN (Empty) (N Red newVal) (Empty), ctx)
nav_ins newVal l@((TN _ (N _ val) _), _) = case compare newVal val of
                                              LT => nav_ins newVal $ left l
                                              EQ => l
                                              GT => nav_ins newVal $ right l

balance: Loc -> Loc
balance l@(TN Empty (Node Red val) Empty, T) = l


insert: Int -> Loc -> Loc
insert v l = let inserted = nav_ins v l in
                      balance inserted

testTree: RBTree
testTree = TN Empty (N Red 1) (TN (TN Empty (N Red 2) Empty) (N Red 3) Empty)


-- insert: Int -> RBTree -> RBTree
-- insert nv Empty = TN Empty (N Red nv) Empty
-- insert nv tn@(TN y n w) = let ins = nav nv (top tn) in
--                                 extract





-- insert nv Empty = N Red Empty nv Empty


-- data RBTree : Type -> Color -> Type where
--   Empty : (Ord elem) =>  (c: Color) -> RBTree elem c
--  rEmptc : lOrd elem) =>  (c: Color) -> RBTree elem c
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
-- succColor: Color -> Color
-- succColor Black = Red
-- succColor Red = Black
