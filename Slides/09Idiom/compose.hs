{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Control.Applicative

gfmap :: forall f g a b. (Functor g, Functor f) 
         => (a -> b) -> g(f(a)) -> g(f(b))
gfmap fun a = mapG (mapF fun) a where
  mapF :: (a -> b) -> f a -> f b
  mapG :: (f a -> f b) -> g (f a) -> g (f b)
  mapF = fmap
  mapG = fmap 
  
  
-- g :: * -> *,  f :: * -> * => g :. f :: * -> *
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Functor g, Functor f) => Functor (g :. f) where
--  fmap fun (O gfa) = O $ fmap (fmap fun) $ gfa
  fmap fun (O gfa) = O $ (fun <$>) <$> gfa
  