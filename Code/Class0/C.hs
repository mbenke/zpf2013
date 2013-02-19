module C where

class C1 a where
  m0 :: a 
  m1 :: a -> a
  
class C1 a => C2 a where
  m2 :: a -> a -> a

