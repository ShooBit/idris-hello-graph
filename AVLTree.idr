-- data Balance = BLeft Balance | BRight Balance | Zero
--
-- data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Empty
--
-- data AVL : Type where
--    AVLTree : (b: Balance) ->
--     (t: BinaryTree) ->
--     AVL
--
-- -- test: AVL Nat
-- -- test = Empty
--
-- -- insert: a -> AVL a -> AVL a
-- -- insert x Empty = Node Zero Empty x Empty
-- -- insert x tree@(Node y z w s) = let inserted = ?insert_binary x tree
data AVLTree = Node Integer AVLTree Integer AVLTree | Empty

insert: Integer -> AVLTree -> AVLTree
insert v (Node b l v' r) = case v < v' of
                              True => Node (-1) (insert v l) v' r
                              False => Node (-1) l v' (insert v r)
insert v Empty = Node 0 Empty v Empty
