module MyParsec2b.Combinators where
import MyParsec2b.Prim
import Data.Char(isSpace, isDigit)

space :: Parser Char
space = satisfy isSpace

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

many, many1 :: Parser a -> Parser [a]
many p  = many1 p <|> return []
many1 p = do { x <- p ; xs <- many p; return (x:xs) }

skipMany :: Parser a -> Parser ()
skipMany p = many p >> return ()  -- można efektywniej
spaces p = skipMany space

thenSkip  :: Parser a -> Parser b -> Parser a
thenSkip p q = do {x <- p; q; return x}

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end  = scan where
  scan = do{ end; return [] }
      <|>
         do{ x <- p; xs <- scan; return (x:xs) }


oneOf, noneOf :: [Char] -> Parser Char
oneOf cs            = satisfy (\c -> elem c cs)
noneOf cs           = satisfy (\c -> not (elem c cs))

sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }
                      
chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x
                      
                      