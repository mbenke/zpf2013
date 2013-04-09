module MyParsec3a
       ( testP,
         parse,
         Parser,
         char,
         (<|>),
         module MyParsec3a.Combinators,
  )where
import MyParsec3a.Prim
import MyParsec3a.Combinators
import Data.Char(isDigit,digitToInt)

parse :: Parser a -> String -> String -> Either ParseError a
parse p fname input = runParser p input 
  (\a st -> Right a) -- cok
  (\e -> Left e)       -- cerr

testP :: Show a => Parser a -> State -> Reply a
testP p input = runParser p input Ok Error
  
p0 = return ()
test0 = testP p0 ""

p2 = item >> item
test1 = testP p2 "" -- expect "EOF"
test2 = testP p2 "abc" -- "'b', c"

p3 :: Parser String
p3 = p <|> q where
  p = char 'p' >> eof >> return "p"
  q = char 'p' >> char 'q' >> eof >> return "q"
test3 = testP p3 "pq"

p4 :: Parser Int
p4 = fmap digitToInt (digit)
test4a = testP p4 "7"
test4b = testP p4 "x"