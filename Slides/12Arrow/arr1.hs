class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d

newtype Kleisli m a b = K(a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f = K(return . f)
  K f >>> K g = K(f >=> g)

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> (f x >>= g)

