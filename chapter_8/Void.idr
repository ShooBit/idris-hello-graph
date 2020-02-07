-- checkEqNat: (n1: Nat) -> (n2: Nat) -> Dec (n1 = n2)
-- checkEqNat Z Z = Yes Refl
-- checkEqNat Z (S k) = ?checkEqNat_rhs_4
-- checkEqNat (S k) Z = ?checkEqNat_rhs_1
-- checkEqNat (S k) (S j) = case checkEqNat k j of
--                           No prf => ?pr
--                           Yes prf => Yes (cong prf)
factorial: (n:Nat)->Nat
factorial Z = 1
factorial n@(S k) = n * (factorial k)

testFactorial1: factorial 3 = 6
testFactorial1 = Refl

testFactorial2: factorial 5 = 120
testFactorial2 = Refl


andb_true_elim_2: (b,c : Bool)->(b && c = True)-> c = True
andb_true_elim_2 False False prf = rewrite prf in Refl
andb_true_elim_2 False True prf = Refl
andb_true_elim_2 True False prf = rewrite prf in Refl
andb_true_elim_2 True True prf = Refl


lte: (n,m:Nat) -> Bool
lte Z Z = True
lte Z (S k) = True
lte (S k) Z = False
lte (S k) (S j) = Main.lte k j

blt_nat: (n,m:Nat)->Bool
blt_nat n m= Main.lte n m && not(Main.lte m n)

testLte1: Main.blt_nat 2 3 = True
testLte1 = Refl

plus_id_exercise : (n, m, o : Nat) -> (n = m) -> (m = o) -> n + m = m + o
plus_id_exercise n m o prf prf1 = rewrite prf in rewrite prf1 in Refl

mult_0_r: (n:Nat) -> n*0 =0
mult_0_r Z = Refl
mult_0_r (S k) = mult_0_r k

plus_n_Sm: (n,m : Nat) -> S (n + m) = n + (S m)
plus_n_Sm Z Z = Refl
plus_n_Sm Z (S k) = Refl
plus_n_Sm (S k) m = let hyp = plus_n_Sm k m in
                  rewrite hyp in Refl


data NatList : Type where
  Nil : NatList
  (::) : Nat -> NatList -> NatList

nonzeros : (l : NatList) -> NatList
nonzeros [] = []
nonzeros (k :: xs) = if k == 0 then
                      nonzeros xs
                      else
                        k::(nonzeros xs)

test_nonzeros : nonzeros [0,1,0,2,3,0,0] = [1,2,3]
test_nonzeros = ?test_nonzeros_rhs
