module Main(main) where
import MyParsec2a.CsvParse
import MyParsec2a(parse)

main = do
  cs <- getContents
  case parse csvFile "stdin" cs of
    Right lines -> print $ countfields lines
    Left e -> print e
  

countfields :: [[a]] -> Int
countfields ss = sum (map length ss)