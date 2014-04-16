{-# LANGUAGE Rank2Types #-}
module Utils.Lens where
import Data.Functor

type SimpleLens a b = forall f.(Functor f) =>  (b -> f b) -> (a -> f a)

-- Functor f =>  (a->b) -> (a->b->a) -> (b->f b) -> (a->f a)
lens :: (a->b) -> (a->b->a) -> SimpleLens a b
lens ab abb bfb a = abb a <$> bfb (ab a)  

newtype Getting b a = Getting { got :: b }
-- Getting :: b -> Getting b a
-- got :: Getting b a -> b
instance Functor (Getting b) where
  fmap _ (Getting b) = Getting b

view :: SimpleLens a b -> a -> b
view l x = got (l Getting x)
infixl 8 ^.
x ^. l = got (l Getting x)

newtype Setting a = Setting { unsetting :: a }
instance Functor Setting where
  fmap f (Setting a) = Setting (f a)

adjust :: SimpleLens a b -> (b -> b) -> a -> a
adjust l m = unsetting . l (Setting . m)
infixl 4 %~
(%~) = adjust

set :: SimpleLens a b -> b -> a -> a
set l x = adjust l (const x)
infixl 4 ^~
(^~) = set