import Data.Vect

data EqNat: Nat -> Nat -> Type where
  Same: (num: Nat) -> EqNat num num

sameS: {k:Nat} -> {j:Nat} -> (eq:EqNat k j) -> EqNat (S k) (S j)
sameS {k = j} {j = j} (Same j) = Same (S j)


checkEqualNat: (num1:Nat) -> (num2:Nat) -> Maybe (EqNat num1 num2)
checkEqualNat Z Z = Just (Same Z)
checkEqualNat Z (S k) = Nothing
checkEqualNat (S k) Z = Nothing
checkEqualNat (S k) (S j) = case checkEqualNat k j of
                                Nothing => Nothing
                                Just eq => Just (sameS eq)


exactLength: {m:Nat}->(len: Nat)->(Vect m a)->Maybe (Vect len a)
exactLength {m} len xs = case checkEqualNat m len of
                          Nothing => Nothing
                          Just (Same eq) => Just xs

the': (a:Type) -> a -> a
the' a x = x
