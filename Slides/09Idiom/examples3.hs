import Control.Monad(ap)

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

f <$> x = pure f <*> x

{-
instance Monad m => Applicative m where
  pure = return
  (<*>) = ap
-}

instance Applicative Maybe where
  pure = return
  (<*>) = ap

instance Applicative IO where
  pure = return
  (<*>) = ap

instance Applicative [] where
  pure = (:[])
  fs <*> xs = concat $ for fs (for xs)
  
for = flip map

mif c t e = do { b <- c; if b then t else e }

aif fc ft fe = cond <$> fc <*> ft <*> fe where
  cond c t e =if c then t else e

main = do
  putStrLn "Monad:"
  mif (return True) (putStrLn "True") (putStrLn "False")
  putStrLn "Idiom:"
  aif (pure True) (putStrLn "True") (putStrLn "False")
