interface TreeEntry a where
  toTreeEntry : a -> Nat

TreeEntry String where
  toTreeEntry s = length s

TreeEntry Nat where
  toTreeEntry a = a

data Tree: Nat -> Type where
  Empty: Tree Z
  Node: Tree a -> Nat -> Tree b -> Tree (S(Nat.maximum a b))

newHeight : Nat -> Tree h -> Nat
newHeight nv (Empty) = (S Z)
newHeight nv (Node {a} l v {b} r) = case compare nv v of
                LT: S (newHeight nv r)

insert: Nat a -> (tree: Tree h) -> Tree (newHeight n t)


-- data Tree = N Tree Nat Tree | Empty

--
-- insert: TreeEntry a => a -> Tree-> Tree
-- insert nv Empty = N Empty (toTreeEntry nv) Empty
-- insert nv (N l v r) with (compare (toTreeEntry nv) v)
--         |LT = N (insert nv l) v r
--         |EQ = (N l v r)
--         |GT = N l v (insert nv r)
