-- data Tree : Type -> Type where
--   Empty: (Ord elem) =>  Tree elem
--   Node1: (Ord elem) =>  Tree elem -> elem -> Tree elem -> Tree elem
--   Node2: (Ord elem) =>  Tree elem -> elem -> Tree elem -> elem -> Tree elem -> Tree elem
--   Node3: (Ord elem) =>  Tree elem -> elem -> Tree elem -> elem -> Tree elem -> elem -> Tree elem -> Tree elem

-- data NodeData = Node Int
--
-- NodeType: (numvals: Nat) -> type
-- NodeType Z = NodeData
-- NodeType (S k) = Node Int -> NodeType k


-- insert: (Ord elem)=> elem -> Tree elem -> Tree elem
-- insert new Empty = Node1 Empty new Empty
-- insert new n@(Node1 lt mv rt) = case compare new mv of
--                                           LT => Node1 (insert new lt) mv rt
--                                           EQ => n
--                                           GT => Node1 lt mv (insert new rt)
-- insert new (Node2 lt lv mt rv rt) = case compare new lv of
--                                          case_val => ?insert_rhs_3
-- insert new (Node3 lt lv mlt mv mrt rv rt) = ?insert_rhs_4


AdderType: (numargs: Nat) -> Type -> Type
AdderType Z numtype = numtype
AdderType (S k) numtype = (next: numtype) -> AdderType k numtype

adder: Num numtype => (numargs: Nat) -> numtype -> AdderType numargs numtype
adder Z x = x
adder (S k) x = \next => adder k (next + x)
