module Rel where

Pred : Set → Set1
Pred A = A → Set

Rel : Set → Set1
Rel A = A → A → Set

Reflexive : {A : Set} → (R : Rel A) → Set
Reflexive {A} R = (a : A) → R a a

Substitutive : {A : Set} → Rel A → Set1
Substitutive {A} R = (x y : A) → (P : Pred A) → R x y → P x → P y

Symmetric : {A : Set} → Rel A → Set
Symmetric {A} R = (x y : A) → R x y → R y x

forceSym : {A : Set} → (R : Rel A) → Reflexive R → Substitutive R → Symmetric R
forceSym R refl subst m n p = subst m n (λ x → R x m) p (refl m)