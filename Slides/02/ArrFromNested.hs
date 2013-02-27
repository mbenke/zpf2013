{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module ArrFromNested where
import Data.Array

type family ListOfIndex i a
-- type instance ListOfIndex () a = a
type instance ListOfIndex Int a = [a]
type instance ListOfIndex (Int, i) a = [ListOfIndex i a]

class Ix i => ArrConv i where
  acArgs :: ListOfIndex i a -> ((i, i), [a])

-- instance ArrConv () where
--  acArgs x = (((), ()), [x])

instance ArrConv Int where
  acArgs xs = ((0,length xs -1),xs)
  
instance ArrConv i => ArrConv (Int, i) where
  acArgs lst =
    (((0, inStart), (length lst - 1, inEnd)), args >>= snd)
    where
      args = map acArgs lst
      (inStart, inEnd) = fst (head args)

arrFromNestedLists :: ArrConv i => ListOfIndex i a -> Array i a
arrFromNestedLists = uncurry listArray . acArgs
