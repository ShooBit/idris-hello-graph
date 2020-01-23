data Balance = LE | BA | RI

balance: Nat -> Nat -> Balance
balance Z Z = BA
balance Z (S k) = LE
balance (S k) Z = RI
balance (S k1) (S k2) = balance k1 k2

data AVLTree: (height: Nat) -> Type where
  Empty: AVLTree 0
  Node: AVLTree b-> Int -> AVLTree a-> AVLTree (S (maximum a b))

treeHeight: AVLTree height-> Nat
treeHeight {height} _= height

data Ctx = L (AVLTree height) Int Ctx | R Ctx Int (AVLTree height) | T

Loc: Nat -> Type
Loc level = (AVLTree level, Ctx)


left: Loc height -> (h**Loc h)
left (Empty, ctx) = (_**(Empty, ctx))
left (Node l v r, ctx) = (_**(l, L r v ctx))

up: Loc height -> (h**Loc h)
up (t, T) = (_**(t, T))
up (l, (L r v ctx)) = (_**(Node l v r, ctx))
up (r, (R ctx v l)) = (_**(Node l v r, ctx))


-- left ((Node l v r), ctx) = (l, L r ctx)



-- insert_leaf: Int ->  Loc (S level) -> Loc level
-- insert_leaf nv (t, L Empty ) =



-- insert: Int -> AVLTree height -> Either (AVLTree (S height)) (AVLTree height)
-- insert nv Empty = Left (Leaf nv)
-- insert nv l@(Leaf v) = case compare nv v of
--                         LT => Left (Node (Leaf nv) v l)
--                         EQ => Right l
--                         GT => Left (Node l v (Leaf nv))
-- insert nv n@(Node l v r) = case compare nv v of
--                         LT => insert nv l
--                         EQ => Right n
--                         GT => insert nv r


-- Loc: Type
-- Loc = (RBTree, Cxt)
--
-- left: Loc -> Loc
-- left (TN l n r, c) = (l, L r n c)
--
-- right: Loc -> Loc
-- right (TN l n r, c) = (r, R c n l)
--
-- up: Loc -> Loc
-- up (l, L r n c) = (TN l n r, c)
-- up (r, R c n l) = (TN l n r, c)
--
-- top: RBTree -> Loc
-- top t = (t,T)
--
-- modify: (RBTree -> RBTree) -> Loc -> Loc
-- modify f (t, ctx) = (f t, ctx)
