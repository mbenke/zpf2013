{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

class (Elem c ~ a) => Collection c a where
  type Elem c
  empty :: c
  insert :: a -> c -> c
  member :: a -> c -> Bool

instance Eq a => Collection [a] a where
  type Elem [a] = a
  empty = []
  insert  = (:)
  member = elem

-- ins2 :: Collection c a => a -> a -> c -> c
ins2 x y c = insert y (insert x c)

-- tu sygnatura niezbędna
-- noproblem :: (Collection c Char, Elem c ~ Char) => c
noproblem :: [Char]
noproblem = ins2 'a' 'b' empty
