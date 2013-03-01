module Nat where
open import Equality

data Nat : Set  where
  zero : Nat
  suc : Nat → Nat

{-# BUILTIN NATURAL Nat #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC suc #-}

infixl 6 _+_
_+_ : Nat → Nat → Nat
zero + y = y
suc x + y = suc (x + y)

thmPlusZero : ∀(n : Nat) → n + 0 ≡ n   -- ∀ n ∈ N . n + 0 = n
thmPlusZero zero = refl
thmPlusZero (suc y) = mapId suc (y + zero) y (thmPlusZero y)

open EqReasoning Nat
thmPlusZero2 : ∀(n : Nat) → n + 0 ≡ n   -- ∀ n ∈ N . n + 0 = n
thmPlusZero2 zero = begin zero =[ refl ] 0 end
thmPlusZero2 (suc y) = 
  begin suc y + 0
  =[ refl ] suc (y + zero) 
  =[ mapId suc (y + 0) y (thmPlusZero2 y) ] suc y 
  end 
  -- where open EqReasoning Nat
