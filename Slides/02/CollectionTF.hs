{-# LANGUAGE TypeFamilies #-}

class Collection c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool

instance Eq a => Collection [a] where
  type Elem [a] = a
  empty = []
  insert  = (:)
  member = elem

-- ins2 :: Collection c => Elem c -> Elem c -> c -> c
ins2 x y c = insert y (insert x c)

-- tu sygnatura niezbędna
-- noproblem :: (Collection c, Elem c ~ Char) => c
noproblem :: [Char]
noproblem = ins2 'a' 'b' empty
