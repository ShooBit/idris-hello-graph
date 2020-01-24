-- -- between: Nat -> Nat -> Nat -> Nat -> Nat
-- -- between v Z Z Z = v
-- -- between v (S k) (S l) = less v k k1
-- -- data Tuple = Maybe Nat
-- --
-- minMaybe:  Maybe Nat -> Maybe Nat -> Maybe Nat
-- minMaybe  (Just x) (Just y) = Just (Nat.minimum x y)
-- minMaybe  Nothing (Just x) = Nothing
-- minMaybe  (Just x) Nothing = Nothing
-- minMaybe  Nothing Nothing = Nothing
--
-- maxMaybe:  Maybe Nat -> Maybe Nat -> Maybe Nat
-- maxMaybe  (Just x) (Just y) = Just (Nat.maximum x y)
-- maxMaybe  Nothing (Just x) = Nothing
-- maxMaybe  (Just x) Nothing = Nothing
-- maxMaybe  Nothing Nothing = Nothing


data Graph:Nat -> Type where
  empty: Graph 0
  merge: (n:Nat) -> Graph (S n) -> Graph (S n) -> Graph (n) 



insert: Graph k -> Node k -> Graph
insert empty y = ?insert_rhs_1
insert (connect x z) y = ?insert_rhs_2

-- data Tree:  Nat -> Type where
--   Empty: Tree 0
--   Node: (Tree a) -> (Tree b) -> Tree (S (Nat.maximum a b))
--
-- node: (h: Nat) -> Tree h

--
-- data Tree : (Maybe Nat) -> Type where
--   Empty : Tree Nothing
--   Leaf: (a: Maybe Nat) -> Tree a
--   Fork : (a: Maybe Nat) -> (b: Maybe Nat) -> (c: Maybe Nat) ->
--     Tree (get_tree_val Nat.minimum b c) ->  Tree (get_tree_val Nat.maximum b c) -> Tree a

-- calc_type: Maybe Nat -> Maybe Nat
-- case Empty =
-- calc_type: Nat -> Maybe Nat-> Maybe Nat
-- calc_type k Nothing = Just k
-- calc_type _ (Just k) = Just k
--
-- data Tree: (Maybe Nat) -> Type where
--   Empty :  Tree Nothing
--   Fork : (a: Maybe Nat) -> (b: Maybe Nat) -> (c: Maybe Nat)
--           ->  Tree (Main.minMaybe b a) -> Tree (Main.maxMaybe c a) -> Tree a
--
--
--
-- -- insert_left: Nat -> Tree a -> Tree (Main.min )
--
-- insert: (k:Maybe Nat) -> Tree a -> Tree a
-- insert nv Empty = Fork (Just nv) Nothing Nothing Empty Empty
-- insert (Just nv) (Fork (Just v) lv rv l r) = case compare nv v of
--                                 LT => Fork (Just v) (Just nv) rv (insert nv l) r






-- insert: Nat -> Tree a -> Tree a
-- insert a Empty  = Fork (Just Nat) Nothing Nothing (Leaf Nothing) (Leaf Nothing)

-- insert : Nat -> Tree a -> Tree a
-- insert nv {a} Empty v = Node
