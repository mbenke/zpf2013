import Test.QuickCheck
import qualified Data.List as L

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) 
       | x <= y = x:y:ys
       | otherwise = y:insert x ys

insort [] = []
insort (x:xs) = insert x $ insort xs

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

prop_insert1 x xs = ordered (insert x xs)

prop_insert2 x xs = ordered xs ==> ordered (insert x xs)

prop_insert3 x xs = collect (length xs) $  ordered xs ==> ordered (insert x xs)

data OrderedInts = OrderedInts [Int] deriving (Eq,Show)

prop_insert4 :: Int -> OrderedInts -> Bool
prop_insert4  x (OrderedInts xs) = ordered (insert x xs)

instance Arbitrary OrderedInts where
  arbitrary = do
    xs <- arbitrary
    return . OrderedInts . L.sort $ xs