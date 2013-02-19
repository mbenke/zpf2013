module B where
data B = F | T

n :: B -> B
n T = F
n F = T

class E a where
  eq :: a -> a -> B
  
instance E B where 
  eq x y = case x of
    T -> y
    F -> case y of 
      T -> F
      F -> T

neq :: E a => a -> a -> B
neq x y = n (eq x y)

{-# NOINLINE test #-}
test  = neq T