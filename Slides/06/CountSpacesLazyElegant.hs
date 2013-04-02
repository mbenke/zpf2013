import Data.Char(isSpace)

countSpaces :: String -> Int
countSpaces = length . filter isSpace

main = do
  s <- readFile "Makefile"
  print $ countSpaces s
