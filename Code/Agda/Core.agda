module Core where

data ⊥ : Set where

⊥-elim : ∀ {Whatever : Set} → ⊥ → Whatever
⊥-elim ()

infix 3 ¬_
¬_ : Set -> Set
¬ A = A → ⊥

record ⊤ : Set where
  constructor tt

data Bool : Set where
  true  : Bool
  false : Bool

{-# BUILTIN BOOL  Bool  #-}
{-# BUILTIN TRUE  true  #-}
{-# BUILTIN FALSE false #-}

{-# COMPILED_DATA Bool Bool True False #-}

------------------------------------------------------------------------
-- Some operations

not : Bool → Bool
not true  = false
not false = true

-- A function mapping true to an inhabited type and false to an empty
-- type.

T : Bool → Set
T true  = ⊤
T false = ⊥

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true  then t else f = t
if false then t else f = f
