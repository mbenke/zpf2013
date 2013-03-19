module mixfix where

data Bool : Set where
  False : Bool
  True  : Bool

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if False then t else e = t
if True then t else e = e

infix 2 _!_+_
_!_+_ : {A : Set} -> Bool -> A -> A -> A
False ! t + e = t
True ! t + e = e

infix 10 !_
!_ : Bool -> Bool
! False = True
! True = False

f : Bool -> Bool -> Bool -> Bool
f x y z = ! x ! y + z