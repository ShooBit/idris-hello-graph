data AVLTree: (height: Nat) -> Type where
  Empty: AVLTree 0
  Leaf: Int -> AVLTree 1
  Node: AVLTree b-> Int -> AVLTree a-> AVLTree (S (maximum a b))

treeHeight: AVLTree height-> Nat
treeHeight {height} _= height

insert: Int -> AVLTree height -> Either (AVLTree (S height)) (AVLTree height)
insert nv Empty = Left (Leaf nv)
insert nv l@(Leaf v) = case compare nv v of
                        LT => Left (Node (Leaf nv) v l)
                        EQ => Right l
                        GT => Left (Node l v (Leaf nv))
insert nv n@(Node l v r) = case compare nv v of
                        LT => insert nv l
                        EQ => Right n
                        GT => insert nv r


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
