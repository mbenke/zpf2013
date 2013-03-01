module Rel2 where

REL : Set → Set → Set1
REL A B = A → B → Set

Rel : Set → Set1
Rel A = REL A A

infixr 4 _⇒_ _=[_]⇒_

-- zawieranie relacji
_⇒_ : {A B : Set} → REL A B → REL A B → Set
R ⇒ S = ∀ {i j} → R i j → S i j

_on_ : ∀ {A : Set} {B : Set} {C : Set1} →
       (B → B → C) → (A → B) → (A → A → C)
_*_ on f = λ x y → f x * f y

_=[_]⇒_ : {A B : Set} → Rel A → (A → B) → Rel B → Set
R =[ f ]⇒ S = R ⇒ S on f -- ∀ x y . R x y ⇒ S (f x) (f y)

Reflexive : {A : Set} → Rel A → Set
Reflexive R = ∀ a → R a a 

_Respects_ : {A : Set} → (A → Set) → Rel A → Set
P Respects _~_ = ∀ x y → x ~ y → P x → P y

Substitutive : {A : Set} → Rel A → Set1
Substitutive {A}  _~_ = (P : A → Set) → P Respects _~_

flip : ∀ {A B C : Set} → (A → B → C) → (B → A → C)
flip f x y = f y x

flip1 : ∀ {A B : Set} → {C : Set1} → (A → B → C) → (B → A → C)
flip1 f x y = f y x

Sym : ∀ {A B} → REL A B → REL B A → Set
Sym R S = R ⇒ flip1 S

Symmetric : ∀ {A} → Rel A → Set
Symmetric R = Sym R R


subst-sym : ∀ {A : Set} → (R : Rel A) → Reflexive R → Substitutive R → Symmetric R
subst-sym R refl subst {m} {n} p = subst (λ x → R x m) m n p (refl m)


-- m n p = subst m n (λ x → R x m) p (refl m)