import Prelude hiding (foldl, foldr, foldl1, foldr1)
import qualified Prelude(foldl, foldr, foldl1, foldr1)
import Data.Monoid

-- Prelude.foldr :: (a -> b -> b) -> b -> [a] -> b

class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m
  -- foldl, foldr',foldl',foldl1,foldr1,...