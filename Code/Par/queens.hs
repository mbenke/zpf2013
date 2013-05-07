type PartialSolution = [Int]
type Solution = PartialSolution
type BoardSize = Int

queens :: BoardSize -> [Solution]
queens n = iterate (concatMap (addQueen n)) [[ ]] !! n

addQueen :: BoardSize -> PartialSolution -> [PartialSolution]
addQueen n s = [x : s | x <- [1..n], safe x s 1]

safe :: Int -> PartialSolution -> Int -> Bool
safe x [] n = True
safe x (c : y) n = x /= c && x /= c + n 
       && x /= c - n && safe x y (n + 1)
