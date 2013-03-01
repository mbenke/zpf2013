module Equality where
open import Core
open import Rel2

infix 4 _≡_ _≢_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

-- Nonequality.

_≢_ : ∀ {A : Set} → A → A → Set
x ≢ y = ¬ x ≡ y

reflexive : ∀ {A : Set} → Reflexive {A} _≡_
reflexive x = refl

substitutive : ∀ {A : Set} → Substitutive {A} _≡_
substitutive P y .y refl py = py

sym : ∀ {A} → Symmetric {A} _≡_
sym = subst-sym _≡_ reflexive substitutive

mapId : ∀ {A  B : Set} → (f : A → B) → (x y : A) → x ≡ y → f x ≡ f y
mapId f .y y refl = refl

module EqReasoning(Carrier : Set) where

  infix  4 _IsRelatedTo_
  infix  2 _end
  infix  2 _∎ -- C-u C-x = \qed
  infixr 2 _=[_]_
  infix  1 begin_

  data _IsRelatedTo_  (x y : Carrier) : Set where
    relTo : (xy : x ≡ y) → x IsRelatedTo y

  begin_ : ∀ {x y} → x IsRelatedTo y → x ≡ y
  begin (relTo xy) = xy

  _∎ : ∀ x → x IsRelatedTo x
  _∎ _ = relTo refl


  _end : ∀ x → x IsRelatedTo x
  _end = _∎ 

  trans : ∀{x y z : Carrier} → x ≡ y → y ≡ z → x ≡ z
  trans refl refl = refl

  _=[_]_ : ∀ x {y z} → x ≡ y → y IsRelatedTo z → x IsRelatedTo z
  _ =[ x∼y ]  relTo y∼z = relTo (trans x∼y y∼z)
