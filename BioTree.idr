NodeType: (numchilds: Nat) -> Type
NodeType Z = Int
NodeType (S k) = (next: Int) -> NodeType k

data BTree: (numnode: Nat) -> NodeType -> Type where
  Empty: BTree Z elem
  Node: BTree (S k) elem
