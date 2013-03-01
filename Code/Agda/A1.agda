module A1 where

{-
Zamiast * : □ mamy Set : Set1 : Set2 : ...

* : Set1
* = Set

□ : Set2
□ = Set1
-}

data Nat : Set  where
  zero : Nat
  suc : Nat → Nat

id' : (A : Set) → A → A
id' A a = a

id : {A : Set} → A → A
id a = id' _ a

id'' : {A : Set} → A → A
id'' {A} a = id {A} a

one : Nat
one = id(suc zero)

two : Nat
two = suc one

infixl 6 _+_
_+_ : Nat → Nat → Nat
zero + y = y
suc x + y = suc (x + y)

{-# BUILTIN NATURAL Nat #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC suc #-}

data False : Set where -- puste

data True : Set where
  tt : True

⊥ : Set    -- \bot
⊥ = False

exfalso : { A : Set } -> ⊥ -> A
exfalso ()

¬_ : Set -> Set
¬ A = A → ⊥

dneg : {A : Set} -> A -> ¬(¬ A)
dneg a = λ nota → nota a

data _∧_ (A B : Set) : Set where
  〈_,_〉 : A → B → (A ∧ B)

π1 : {A B : Set } -> A ∧ B → A
π1 〈 x , y 〉 = x 

both : {A B C : Set} → (C → A) → (C → B) → (C → (A ∧ B))
both f g c = 〈 (f c) , (g c) 〉

-- ćwiczenie: (\or)
data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B

sel : ∀ {A B C : Set} → (A → C) → (B → C) → (A ∨ B) → C
sel fl fr x = {!!}


infix 1 _==_
_==_ : Nat -> Nat -> Set
zero  == zero  = True
zero  == suc m = False
suc n == zero  = False
suc n == suc m = n == m

thmPlusZero : (n : Nat) ->  n + 0 == n   -- ∀ n ∈ N . n + 0 = n
thmPlusZero  zero   = tt
thmPlusZero (suc n) = thmPlusZero n

-- zwrotnosc; NB  n moglo by byc ukryte, ale to wiecej komplikuje niz pomaga
refl : (n : Nat) → n == n
refl zero = tt
refl (suc y) = refl y

subst : (m n : Nat) → (P : Nat → Set) → (pe : m == n) → P m → P n
subst zero zero P pe pm = pm
subst zero (suc y) P () pm
subst (suc y) zero P () pm
subst (suc m') (suc n') P pe pm = subst m' n' (λ x → P (suc x))  pe pm

cong : (f : Nat -> Nat) -> {x y : Nat} -> x == y -> f x == f y
cong f {x} {y} pe = subst x y (λ t ->  (f x == f t) ) pe (refl (f x)) 

sym : (m n : Nat) → (m == n) → (n == m)
sym m n p = subst m n (λ x → x == m) p (refl m)

thmPlusZero2 : (n : Nat) ->  n == n + 0 
thmPlusZero2 n = sym (n + zero) n (thmPlusZero n)

-- ćwiczenie: przechodniość
trans : (x y z : Nat) -> x == y -> y == z -> x == z
trans x y z pxy pyz = {!!}

via : {x : Nat} -> (y : Nat) -> {z : Nat} -> x == y -> y == z -> x == z
via {x} y {z} pxy pyz = trans x y z pxy pyz


thmZero+ : (n : Nat) ->  zero + n == n 
thmZero+ n  = refl n

thmSuc+ : (m n : Nat) -> m + suc n == suc (m + n)
thmSuc+ m n = {!!}

-- ćwiczenie: przemienność dodawania
thmComm+ : ∀ m n -> m + n == n + m
thmComm+ zero n = sym (n + zero) n (thmPlusZero n)
thmComm+ (suc m) n = ? where
   lhs : Nat
   lhs = suc m + n
   step1 : Nat
   step1 = suc(n + m)
   rhs : Nat
   rhs = n + suc m
   lemma :  suc (n + m) == n + suc m
   lemma = sym {!!} {!!} {!!}

data Vec (A : Set) : Nat → Set where
  vnil : Vec A zero
  vcons : {n : Nat} → A → Vec A n → Vec A (suc n)

vhead : {A : Set} → {n : Nat} → Vec A (suc n) → A
vhead (vcons y y') = y