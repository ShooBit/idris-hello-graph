data Color = Black | Red

succColor: Color -> Color
succColor Black = Red
succColor Red = Black



data RBTree : Type -> Color -> Type where
  Empty : (Ord elem) =>  (c: Color) -> RBTree elem c
  Node : (Ord elem) => (c: Color) ->
              RBTree elem (succColor c) ->
              elem ->
              RBTree elem (succColor c) ->
              RBTree elem (succColor c)


test: RBTree Int Black
test = Node Red (Empty Black) 1 (Empty Black)
