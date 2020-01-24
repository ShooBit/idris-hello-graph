data Tree: Type where
  Empty: Tree
  L1: Int -> Tree
  N2: Tree -> (val: Int) -> Tree -> (val: Int) -> Tree -> Tree
  L2: Int -> Int -> Tree
  N3: (val: Int)-> Tree -> (val: Int) -> Tree -> (val: Int) -> Tree -> Tree

data Ctx = T
          |M Tree v Tree Ctx
          | LM Tree v Tree v Tree Ctx
          | MM Tree v Tree v Tree Ctx
          | RM Tree v Tree v Tree Ctx

Loc: Type
Loc = (Tree, Ctx)

insert': Int -> Loc -> Loc
insert' nv (Empty, ctx) = (N1 Empty nv Empty, ctx)
insert' nv ((L1 v), ctx) = case compare nv v of
                        LT => (L2 nv v, ctx)
                        EQ => (L1 v, ctx)
                        GT => (L2 v nv, ctx)
insert' nv ((L2 l r), ctx) = case compare nv v of
                        LT => (N2 Empty nv Empty v Empty, ctx)
                        EQ => (N1 Empty v Empty, ctx)
                        GT => (N2 Empty v Empty nv Empty, ctx)
-- insert' nv (Empty, _) = (N1 Empty nv Empty, T)

extract: Loc -> Tree

insert: Int -> Tree -> Tree
insert nv t = let it = insert' nv (t, T) in
                  extract it
