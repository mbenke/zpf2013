-- module StrictState where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f m = State $ \s -> case runState m s of
                                 (a, s') -> (f a, s')
instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> case runState m s of
                                 (a, s') -> runState (k a) s'
