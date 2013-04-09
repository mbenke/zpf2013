module MyParsec2c.Prim where
import Control.Monad

type ParseError = String
unexpected :: String -> ParseError
unexpected s = "unexpected " ++ s

expected :: String -> ParseError
expected s = "expected " ++ s

unknownError :: ParseError
unknownError = "Unknown error"

type State = [Char]
data Reply a = Ok a State | Error ParseError  
  deriving Show
data Consumed a = Consumed (Reply a)
                | Empty (Reply a)

newtype Parser a = Parser { runParser :: State -> Consumed a }
                    
item = Parser item0 where
  item0 [] = Empty(Error $ unexpected "EOF")
  item0 (x:xs) = Consumed (Ok x xs)

eofWith :: a -> Parser a
eofWith a = eof >> return a

eof = Parser eof' where
  eof' [] = Empty(Ok () [])
  eof' _ = Empty $ Error (expected "EOF")

char :: Char -> Parser Char
char c = (satisfy (==c)) 

satisfy p = Parser sat' where 
  sat' []    = Empty(Error (expected "EOF")) -- or check (p EOF)
  sat' (a:s) = if (p a) then Consumed(Ok a s) else 
                 Empty(Error (unexpected $ show a) )
    
instance Monad Parser where
  return a = Parser $ \s -> Empty(Ok a s)
  
  p >>= k = Parser $ \st -> case runParser p st of
      Empty reply -> case reply of
        Ok a s' -> runParser (k a) s'
        Error e -> Empty $ Error e
      Consumed reply -> Consumed (case reply of
        Ok a s' -> case runParser (k a) s' of
                      Empty r -> r
                      Consumed r -> r
        Error e -> Error e)

instance Functor Parser where
  fmap f p = do { x <- p; return (f x) }
  
parserZero :: Parser a
parserZero = Parser $ \s -> Empty (Error unknownError)

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s -> case runParser p s of
  Empty (Error e) -> runParser q s
  Empty ok -> Empty ok
  consumed -> consumed
  
(<|>) = parserPlus  

instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus
