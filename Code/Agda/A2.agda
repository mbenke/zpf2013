module A2 where
open import Core
open import Nat
open import Equality

infixr 5 _∷_
data Vec (A : Set) : Nat → Set where
  [] : Vec A zero
  _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)

vhead : {A : Set} → {n : Nat} → Vec A (suc n) → A
vhead (h ∷ t) = h

vtail : {A : Set} → {n : Nat} → Vec A (suc n) → Vec A n 
vtail (h ∷ t) = t

vht : ∀ {A n} → (v : Vec A (suc n)) → v ≡ vhead v ∷ vtail v
vht (_ ∷ _) = refl

_++_ : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
[] ++ v2 = v2
(h ∷ t) ++ v2 = h ∷ (t ++ v2)
