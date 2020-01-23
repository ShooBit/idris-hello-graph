data Balance = LL | LE | BA | RI | RR

data AVLTree: (height: Nat) -> Type where
  Empty: AVLTree 0
  Node: AVLTree b-> Int -> AVLTree a-> AVLTree (S (maximum a b))

calc_balance: AVLTree hl -> AVLTree hr -> Balance
calc_balance {hl = Z}        _ {hr = Z}         _ = BA
calc_balance {hl = (S Z)}    _ {hr = Z}         _ = LL
calc_balance {hl = Z}        _ {hr = (S Z)}     _ = RI
calc_balance {hl = (S(S Z))} _ {hr = Z}         _ = LL
calc_balance {hl = Z}        _ {hr = (S (S Z))} _ = RR
calc_balance tl                tr                 = calc_balance tl tr
  -- calc_balance Z (S k) = LE
  -- calc_balance (S k) Z = RI
  -- calc_balance (S k1) (S k2) = calc_balance k1 k2

treeHeight: AVLTree height-> Nat
treeHeight {height} _= height

data Ctx = L (AVLTree height) Int Ctx | R Ctx Int (AVLTree height) | T

Loc: Type
Loc = ((n ** AVLTree n), Ctx)

left: Loc -> Loc
left ((_ ** Empty), ctx) = ((_**Empty), ctx)
left ((_ ** Node l v r), ctx) = ((_**l), L r v ctx)

right: Loc -> Loc
right ((_ ** Empty), ctx) = ((_**Empty), ctx)
right ((_ ** Node l v r), ctx) = ((_**r), R ctx v l)

up: Loc -> Loc
up ((_ **t), T) = ((_**t), T)
up ((_**l), (L r v ctx)) = ((_**Node l v r), ctx)
up ((_**r), (R ctx v l)) = ((_**Node l v r), ctx)

balance_left: Loc -> Loc
balance_left (_ ** Node l v r, ctx) = case calc_balance l 


balance: Loc -> Loc
balance ((_**Node l v r), T) = case calc_balance l r of
                                    BA => balance ((_ ** Node l v r), T)
                                    RI => balance ((_ ** Node l v r), T)
                                    LE => balance ((_ ** Node l v r), T)
                                    LL => balance_left ((_ ** Node l v r), T)


balance ((_ ** Empty), ctx) = balance $ up ((_ ** Empty), ctx)
balance ((_ ** Node l v r), ctx) = case calc_balance l r of
                                    BA => balance $ up ((_ ** Node l v r), ctx)
                                    RI => balance $ up ((_ ** Node l v r), ctx)
                                    LE => balance $ up ((_ ** Node l v r), ctx)

insert:  Int -> Loc -> Loc
insert  nv ((_**Empty), ctx) = balance ((_**(Node Empty nv Empty)), ctx)
insert  nv ((_ ** Node l v r), ctx) = case compare nv v of
                              LT => insert nv $ left ((_ ** Node l v r), ctx)
                              EQ => ((_ ** Node l v r), ctx)
                              GT => insert nv $ right ((_ ** Node l v r), ctx)
