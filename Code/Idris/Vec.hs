{-# OPTIONS -fdefer-type-errors #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
data Nat = Zero | Suc Nat

data Vec :: * -> Nat -> * where
  VNil :: Vec a 0
  VCons :: a -> Vec a n -> Vec a (Suc n)   
-- cannot derive Show

vtolist :: Vec a n -> [a]  
vtolist VNil = []
vtolist (VCons x xs) = x:vtolist xs

add :: Nat -> Nat -> Nat
add Zero y = y
add (Suc x) y = Suc (add x y)

--  The vap does not work
{-
vap :: Vec a n -> Vec a m -> Vec a (add n m)
vap VNil ys = ys
vap (VCons x xs) ys = VCons x (vap xs ys)
-}

va = VCons 'a' VNil

main = print (vtolist $ vap va va)