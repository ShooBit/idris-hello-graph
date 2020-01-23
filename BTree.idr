data Tree : Nat -> Type where
  Empty : (a: Nat) -> Tree a
  Fork : (a: Nat) -> Tree l -> Tree l -> Tree a

insert : Nat -> Tree a -> Tree a
insert nv {a} Empty v = Node
