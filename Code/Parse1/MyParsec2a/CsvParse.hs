module MyParsec2a.CsvParse where
import MyParsec2a.Prim
import MyParsec2a.Combinators

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: Parser [[String]]
csvFile =  manyTill line eof

-- Each line contains 1 or more cells, separated by a comma
line :: Parser [String]
line = cells `endBy` eol
       
cells = cellContent `sepBy` char ','

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: Parser String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: Parser Char
eol = char '\n'