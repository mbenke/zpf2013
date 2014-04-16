{-# LANGUAGE RankNTypes #-}
import Control.Monad.Cont

ex1 :: Cont r Int
ex1 = do
  a <- return 1
  b <- return 10
  return (a+b)
  
-- test :: (forall r. (Show r) => Cont r Int) -> String 
test ex = runCont ex show


-- To samo, jawnie:
-- cont :: ((a->r)->r) -> Cont r a
ex2 :: Cont r Int
ex2 = do
  a <- return 1
  b <- cont (\c -> c 10)
  return (a+b)
  
ex3 = do
   a <- return 1
   b <- cont (\c -> "escape")
   return $ a+b

escape :: r -> Cont r a
escape r = cont (const r)

ex3e = do
   a <- return 1
   b <- escape "escape"
   return $ a+b

ex4 = do
   a <- return 1
   b <- cont (\c -> c 10 ++ c 20)
   return $ a+b
   
test5 = do 
  a <- return 1
  b <- [10, 20]
  return $ a+b   
  
ex6 = do
  a <- return 1
  b <- cont (\c -> c 10 ++ c 20)
  return $ a+b

test6 = runCont ex6 (\x -> [x])

ex7 = do
   a <- return 1
   b <- cont (\c -> concat [c 10, c 20])
   return $ a+b

test7 = runCont ex7 (\x -> [x])

ex8 = do
  a <- return 1
  b <- cont (\c -> [10,20] >>= c)
  return $ a+b

test8 = runCont ex8 return
