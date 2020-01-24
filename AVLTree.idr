data Balance = LL | LE | BA | RI | RR

calc_bal: Nat -> Nat -> Balance
calc_bal Z Z = BA
calc_bal (S Z) Z = LE
calc_bal Z (S Z) = RI
calc_bal (S (S Z)) Z = LL
calc_bal Z (S (S Z)) = RR
calc_bal (S hl) (S hr) = calc_bal hl hr


data AVLTree: (height: Nat) -> (balance: Balance) -> Type where
  Empty: AVLTree 0 BA
  Node: AVLTree a bl -> Int -> AVLTree b br -> AVLTree (S (maximum a b)) (calc_bal a b)

treeHeight: AVLTree height _-> Nat
treeHeight {height} _= height

insert: AVLTree hl _ -> Int -> AVLTree hr _ -> AVLTree (S (maximum hr hl)) (calc_bal hr hl)
insert Empty nv Empty = Node Empty nv Empty
insert Empty nv (Node l v r) = insert Empty nv (Empty)
--
-- data Ctx = L (AVLTree height balance) Int Ctx | R Ctx Int (AVLTree height balance) | T
--
-- Loc: Nat -> Balance -> Type
-- Loc h b = (AVLTree h b, Ctx)

-- left: AVLTree (S h) b -> AVLTree h _
-- left (Node l v r) = l
--
-- right: Loc -> Loc
-- right ((_ ** Empty), ctx) = ((_**Empty), ctx)
-- right ((_ ** Node l v r), ctx) = ((_**r), R ctx v l)
--
-- up: Loc -> Loc
-- up ((_ **t), T) = ((_**t), T)
-- up ((_**l), (L r v ctx)) = ((_**Node l v r), ctx)
-- up ((_**r), (R ctx v l)) = ((_**Node l v r), ctx)
--
-- -- balance_left: Loc -> Loc
-- -- balance_left (_ ** Node l v r, ctx) = case c_child_balance l
--
--
-- balance: Loc -> Loc
-- balance ((_**Node l v r), T) = case calc_balance l r of
--                                     BA => balance ((_ ** Node l v r), T)
--                                     RI => balance ((_ ** Node l v r), T)
--                                     LE => balance ((_ ** Node l v r), T)
--                                     -- LL => balance_left ((_ ** Node l v r), T)
--
--
-- balance ((_ ** Empty), ctx) = balance $ up ((_ ** Empty), ctx)
-- balance ((_ ** Node l v r), ctx) = case calc_balance l r of
--                                     BA => balance $ up ((_ ** Node l v r), ctx)
--                                     RI => balance $ up ((_ ** Node l v r), ctx)
--                                     LE => balance $ up ((_ ** Node l v r), ctx)
--
-- insert:  Int -> Loc -> Loc
-- insert  nv ((_**Empty), ctx) = balance ((_**(Node Empty nv Empty)), ctx)
-- insert  nv ((_ ** Node l v r), ctx) = case compare nv v of
--                               LT => insert nv $ left ((_ ** Node l v r), ctx)
--                               EQ => ((_ ** Node l v r), ctx)
--                               GT => insert nv $ right ((_ ** Node l v r), ctx)
