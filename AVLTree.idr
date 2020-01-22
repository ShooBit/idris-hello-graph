data AVLTree: (height: Nat) -> Type where
  Leaf: Int -> AVLTree Z
  Node: AVLTree b-> Int -> AVLTree a-> AVLTree (S (maximum a b))



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
