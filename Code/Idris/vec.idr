module Main
-- There is built-in Vect type
data Vec : Type -> Nat -> Type where
  VNil : Vec a O
  VCons : a -> Vec a n -> Vec a (S n)

vtoList : Vec a n -> List a
vtoList VNil = []
vtoList (VCons x xs) = x::vtoList xs

add : Nat -> Nat -> Nat
add O y = y
add (S x) y = S (add x y)

vap : Vec a n -> Vec a m -> Vec a (add n m)
vap VNil ys = ys
vap (VCons x xs) ys = VCons x (vap xs ys)

va : Vec Char 1
va = VCons 'a' VNil

main : IO ()
main = print (vtoList $ vap va va)