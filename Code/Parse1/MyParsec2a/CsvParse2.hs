module MyParsec2a.CsvParse where
import MyParsec2a

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: Parser [[String]]
csvFile =  manyTill line eof

-- Each line contains 1 or more cells, separated by a comma
line :: Parser [String]
line = cells `endBy` eol
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: Parser [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: Parser [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: Parser String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: Parser Char
eol = char '\n'