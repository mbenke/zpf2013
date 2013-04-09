module MyParsec3a.Comb where
import MyParsec3a.Prim
import Data.Char(isLetter,isDigit)

letter :: Parsec Char
letter = satisfy isLetter

