class Collection c where
  empty :: c
  insert :: e -> c -> c
  member :: e -> c -> Bool

instance Collection [a] where
     empty = []
     insert x xs = x:xs
     member = undefined
