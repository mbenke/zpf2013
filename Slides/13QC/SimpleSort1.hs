import Data.List (permutations)
import SimpleCheck1

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | pred x y  = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
  where (xs,ys) = split zs
        
sort = mergeSort ((<=) :: Int -> Int -> Bool)         

prop_idempotent xs = 
    sort (sort xs) == sort xs
    
prop_permute :: ([a] -> Bool) -> [a] -> Bool
prop_permute prop = all prop . permutations
    