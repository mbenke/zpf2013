module A3 where
open import Equality
open import Core
open import Nat

infixr 5 _∷_
data [_] (A : Set) : Set where
  [] : [ A ]
  _∷_ : A → [ A ] → [ A ]

_++_ : ∀ {A} → [ A ] → [ A ] → [ A ]
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys 

id : ∀ {A : Set} → A → A 
id x = x

_∘_ : ∀  {A B C : Set} → (B → C) → (A → B) → A → C
(f ∘ g) x = f (g x) 

map : ∀ {A B} → (A → B) → [ A ] → [ B ]
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

map-id : ∀{A} xs → map {A} id xs ≡ xs
map-id [] = refl
map-id {A} (x ∷ xs) = mapId {[ A ]} (_∷_ x) (map id xs) xs (map-id xs)

cong : ∀ {A : Set} {B : Set}
       (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong f refl = refl

-- ćwiczenie: 
map-∘ : ∀ {A B C : Set} (f : B → C) (g : A → B) xs 
  → map (f ∘ g) xs ≡ map f (map g xs)
map-∘ f g xs = {!!}

record Eq(A : Set) : Set where
 field  _==_ : A → A → Bool


   
eqBool : Eq Bool 
eqBool = record { _==_ = _==_  } where
    _==_ : Bool → Bool → Bool
    true == true = true
    false == false = true
    _ == _ = false

-- ćwiczenie:
eqNat : Eq Nat 
eqNat = {!!}

elem : ∀ {A : Set} → {{eqA : Eq A}} → A → [ A ] → Bool
elem x [] = false
elem {{eqA}} x (y ∷ ys) 
  = if x == y then true else elem x ys
{-  with x == y | true = true
            ... | false = elem x ys -}
  where _==_ = Eq._==_ eqA

-- magic ;)
elemBool : Bool → [ Bool ] → Bool
elemBool b bs = elem b bs
