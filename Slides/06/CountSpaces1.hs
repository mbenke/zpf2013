import IterateeM
import Data.Char(isSpace)

countSpaces :: Monad m => Iteratee Char m Int
countSpaces = loop 0
  where loop n = getchar >>= check n
        check n Nothing = return n
        check n (Just c) = loop (if isSpace c then n + 1 else n)

getchar :: Monad m => Iteratee el m (Maybe el)
getchar = headM

runCountSpaces fileName = print =<< run =<< enum_file fileName countSpaces

main = runCountSpaces  "Makefile"