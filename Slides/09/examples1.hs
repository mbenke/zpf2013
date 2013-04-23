import Prelude hiding(sequence,zipWith)

sequence1 :: Monad m => [m a] -> m [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)

sequence2 :: Monad m => [m a] -> m [a]
sequence2 [] = return []
sequence2 (c:cs) = return (:) `ap` c `ap` sequence2 cs

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return $ f x

transpose1 :: [[a]] -> [[a]]
transpose1 [xs] = [[x] | x <- xs]
transpose1 (xs:xss) = consAll xs (transpose1 xss) 

consAll :: [a] -> [[a]] -> [[a]]
consAll (y:ys) (zs:zss) = (y:zs):consAll ys zss
consAll _      _        = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (y:ys) (z:zs) = f y z:zipWith f ys zs
zipWith _  _      _      = []

consAll2 ys zss = map (uncurry (:)) $ zip ys zss


transpose2 :: [[a]] -> [[a]]
transpose2 (xs:xss) = zipWith (:) xs (transpose2 xss)
transpose2 [] = []
  
transpose :: [[a]] -> [[a]]
transpose (xs:xss) = repeat (:) `zap` xs `zap` transpose xss
transpose [] = repeat []

zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []

data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

type Env v = [(v,Int)]
fetch :: v -> Env v -> Int
fetch v e = undefined

eval1 :: Exp v -> Env v -> Int
eval1 (Var x) env = fetch x env
eval1 (Val i) env = i
eval1 (Add p q) env = eval1 p env + eval1 q env

eval2 (Var x) = fetch x
eval2 (Val i) = k i
eval2 (Add p q) = k (+) `s` eval2 p `s` eval2 q
k x y = x
s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es env = (ef env) (es env)