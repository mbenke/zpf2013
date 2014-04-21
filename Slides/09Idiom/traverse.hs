import Control.Applicative

distL :: Applicative f => [f a] => f [a]
distL []     = pure []
distL (x:xs) = (:) <$> x <*> distL xs

flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap m as = distL (fmap m as)

-- m :: a -> Maybe b
-- as :: [a]
-- fmap m as :: [Maybe b]

traverseL :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseL f []     = pure []
traverseL f (x:xs) = (:) <$> f x <*> traverseL f xs

class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist = traverse id
  traverse f = dist . fmap f
  
instance Traversable [] where
  traverse = traverseL
  dist = distL