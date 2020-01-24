StringInt: Bool -> Type
StringInt True = String
StringInt False = Int

data Tree = N Tree Int Tree | Empty

insert: (isString:Bool)->StringInt isString -> Tree-> Tree
insert True nv t with (toIntNat (length nv))
        | nn = insert False nn t
insert False nv Empty = N Empty nv Empty
insert False nv (N l v r) with (compare nv v)
        |LT = N (insert False nv l) v r
        |EQ = (N l v r)
        |GT = N l v (insert False nv r)
