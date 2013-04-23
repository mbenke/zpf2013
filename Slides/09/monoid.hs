{-# LANGUAGE TypeOperators #-}

-- For Monoidal "instances"
{-# LANGUAGE FlexibleInstances, OverlappingInstances,UndecidableInstances #-}
import Control.Applicative

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- typ fantomowy: nie używa swojego argumentu
newtype Accy o a = Acc{acc::o}
instance Functor (Accy o) where
  fmap _ (Acc o) = Acc o
  
instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  (Acc o1) <*> (Acc o2) = Acc (o1 <> o2)
  
data Except err a = Ok a | Failed err

instance Functor (Except err) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Failed err) = Failed err
  
instance Monoid err => Applicative (Except err) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Failed err = Failed err
  Failed err <*> Ok _ = Failed err
  Failed e1 <*> Failed e2 = Failed (e1 `mappend` e2)
  
-- g :: * -> *,  f :: * -> * => g :. f :: * -> *
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Functor g, Functor f) => Functor (g :. f) where
  fmap = fmapFF
  
fmapFF :: (  Functor g,   Functor f) => (a -> b) -> (g :. f) a -> (g :. f) b
fmapFF = inO.fmap.fmap

(f ~> h) g = h . g . f
inO :: (g (f a) -> g' (f' a')) -> ((g :. f) a -> (g' :. f') a')
inO = unO ~> O

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  pure  = O . pure . pure
  O gs <*> O xs = -- O (| (<*>) gs xs |) 
                  O ( (<*>) <$> gs <*> xs)
                  
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)
  
instance Applicative f => Monoidal f where
  unit = pure ()
  pair fa fb = (,) <$> fa <*> fb
  
instance Monoidal f => Applicative f where  
  pure x = const x <$> unit
  mf <*> mx = (\(f,x) -> f x) <$> pair mf mx
  -- albo uncurry ($) <$> pair mf mx