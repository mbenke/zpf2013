{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction #-}

-- import Test.QuickCheck

{- class Functor f => Pointed f where
  pure :: a -> f a
-}

class  Functor f => Mona f where
  pure :: a -> f a
  join :: f (f a) -> f a
  
newtype M a = M a
instance Functor M where
  fmap f (M x) = M (f x)
  
instance Mona M where
  pure = M
  join (M (M a)) = M a

-- instance (Eq a, Functor f) => Eq (f a) where
--   f == g = undefined
infix 4 ===

(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
f === g = \x -> f x == g x

instance Eq a => Eq (M a) where
  (M a) == (M b) = a == b
  
prop_pure g = fmap g . pure === pure . g

-- prop_join = join . fmap join === join . join
type A = Int
prop_join = join1 . fmap join2 === join . join4
  where 
    -- l, r :: M(M(M a)) -> M a  
    join1,  join2 :: M(M A) -> M A
    join1 = join
    join2 = join
    join4 :: M(M( M A)) -> M (M A)
    join4 = join