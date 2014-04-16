{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding((*))
class Mul a b where
  type MulTy a b
  (*) :: a -> b -> MulTy a b
  
newtype Vec a = Vec [a]
instance Functor Vec where
  fmap f (Vec as) = Vec $ map f as
  
instance Mul a b => Mul a (Vec b) where
  type MulTy a (Vec b) = Vec (MulTy a b)
  a * b = fmap (a*) b
  
f b x y = if b then  x * (Vec [y]) else y