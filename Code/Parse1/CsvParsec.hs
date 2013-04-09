module Main(main) where
import Text.ParserCombinators.Parsec(parse)
import CsvParse(csvFile)
-- import System.Environment

main = do
  cs <- getContents
  case parse csvFile "stdin" cs of
    Right lines -> print $ countfields lines
    Left e -> print e
  

countfields :: [[a]] -> Int
countfields ss = sum (map length ss)