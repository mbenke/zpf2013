{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
class Collection c e where
  empty  :: c
  insert :: e -> c -> c
  member :: e -> c -> Bool

instance Eq a => Collection [a] a where
  empty = []
  insert  = (:)
  member = elem

ins2 x y = insert y . insert x

-- problem1 :: [Int] 
-- problem1 = ins2 1 2 []


-- problem2 = ins2 'a' 'b' []

problem3 :: (Collection c0 Char, Collection c0 Bool) => c0 -> c0
problem3 = ins2 True 'a'
