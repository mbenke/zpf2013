{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding((*))
class Mul a b c  | a b -> c  where
  (*) :: a -> b -> c
  
newtype Vec a = Vec [a]
instance Functor Vec where
  fmap f (Vec as) = Vec $ map f as
  

instance Mul a b c => Mul a (Vec b) (Vec c) where
  a * b = fmap (a*) b

instance Mul Int (Vec Int) Int where
  x * y = undefined
  
-- f :: (Mul a (Vec b) b) => Bool -> a -> b -> b
f b x y = if b then  x * (Vec [y]) else y
