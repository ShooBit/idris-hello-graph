data Tree: Nat -> Type where
  Empty: Tree 0
  Node: Tree a -> Int -> Tree b -> Tree (S (maximum a b))
