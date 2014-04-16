class Collection c where
  empty :: c e
  insert :: e -> c e -> c e
  member :: Eq e => e -> c e-> Bool

instance Collection [] where
  empty = []
  insert x xs = x:xs
  member = elem

ins2 x y c = insert y (insert x c)
-- ins2 :: Collection c => e -> e -> c e -> c e

problem :: [Int]
problem = ins2 1 2 empty