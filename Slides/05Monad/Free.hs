import Control.Monad
import Control.Monad.Trans

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
   fmap f (Pure a) = Pure (f a)
   fmap f (Free as) = Free (fmap (fmap f) as)
 
instance Functor f => Monad (Free f) where
   return = Pure
   Pure a >>= f = f a -- the first monad law!
   Free as >>= f = Free (fmap (>>= f) as)
   
instance MonadTrans Free where
    lift = Free . liftM Pure