{-# LANGUAGE GADTs #-}
module GVec where
data Zero
data Suc n

data Vec n a where
  Nil :: Vec Zero a
  Cons :: a -> Vec n a -> Vec (Suc n) a
