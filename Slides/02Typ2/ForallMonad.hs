{-# LANGUAGE ExplicitForAll, KindSignatures #-}

f :: forall (m :: * -> *) . (Monad m) => forall (a :: *) . m a -> m a
f = id