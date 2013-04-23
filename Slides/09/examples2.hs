{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances  #-}
import Test.QuickCheck

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

(<$>) :: (Applicative f) => (a -> b) -> f a -> f b
f <$> u = pure f <*> u

instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _ = Nothing
  
-- >>> pure id <*> Just 5
-- Just 5
-- >>> pure (+) <*> Just 2 <*> Just 2
-- Just 4

sequence3 (c:cs) = (:) <$> c <*> sequence3 cs

instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
  
transpose3 :: [[a]] -> [[a]]
transpose3 []       = pure []
transpose3 (xs:xss) = (:) <$> xs <*> transpose3 xss

transpose4 (xs:xss) = return (:) `ap` xs `ap` transpose4 xss
transpose4 [] = return []

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return $ f x

instance Applicative ((->) env) where
  pure = const
  ef <*> es = \env -> (ef env) (es env)
  
data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

type Env v = [(v,Int)]
fetch :: v -> Env v -> Int
fetch v e = undefined
  
eval3 (Var x) = fetch x
eval3 (Val i) = pure i
eval3 (Add p q) = pure (+) <*> eval3 p <*> eval3 q

class Applicative i => Idiomatic i f g | g -> f i where
   idiomatic :: i f -> g
 
iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure
 
data Ii  =  Ii
 
instance Applicative i    => Idiomatic i x (Ii -> i x) where
  idiomatic xi Ii     = xi
 
instance Idiomatic i f g  => Idiomatic i (s -> f) (i s -> g) where
  idiomatic sfi si    = idiomatic (sfi <*> si)

sequence4 (c:cs) = iI (:) c (sequence cs) Ii
eval4  (Add p q) = iI (+) (eval3 p) (eval3 q) Ii