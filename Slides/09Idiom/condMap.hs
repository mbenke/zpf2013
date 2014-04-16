import Control.Applicative
import Data.Monoid
import Data.Either

data Except err a = Ok a | Failed err 
                  deriving Show

instance Functor (Except err) where
  fmap f (Ok a) = Ok (f a)
  fmap f (Failed e) = Failed e
  -- fmap f failed = failed -- gupi tajpczeker
  
instance Monoid err => Applicative (Except err) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Failed err = Failed err
  Failed err <*> Ok _ = Failed err
  Failed e1 <*> Failed e2 = Failed (e1 `mappend` e2)

-- condMap p f xs | all p xs = map f xs
--                | otherwise = blad

condMap1 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap1 p f (x:xs) 
  | p x = do ys <- condMap1 p f xs
             return $ f x:ys
  | otherwise = Left x                 
condMap1 p f [] = return []

-- > condMap1 even (+1) [2,4]
-- Right [3,5]
-- > condMap1 even (+1) [2,3,4]
-- Left 3

condMap2 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap2 p f (x:xs) 
  | p x = (f x:) <$> condMap2 p f xs
  | otherwise = Left x                 
condMap2 p f [] = return []

condMap3 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap3 p f (x:xs) = do
  y <- f <$> check p x
  ys <- condMap3 p f xs
  return (y:ys)
  where check p x = if p x then Right x else Left x
condMap3 p f [] = return []

condMap4 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap4 p f (x:xs) = (:) <$> ay <*> ays  where
  ay  = f <$> check p x
  ays = condMap4 p f xs
  check p x = if p x then Right x else Left x
condMap4 p f [] = pure []

-- f  $  g  $  x === f . g  $ x    
-- f <$> g <$> x === f . g <$> x

condMap5 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap5 p f (x:xs) = (:) . f <$> (check p x) <*> condMap5 p f xs 
  where check p x = if p x then Right x else Left x
condMap5 p f [] = pure []


condMap6 :: (a -> Bool) -> (a->b) -> [a] -> Except [a] [b]
condMap6 p f (x:xs) = (:) . f <$> (check p x) <*> condMap6 p f xs 
  where check p x = if p x then Ok x else Failed [x]
condMap6 p f [] = pure []

condEven :: [Int] -> Except [Int] [Int]
condEven = condMap6 even (+1)
