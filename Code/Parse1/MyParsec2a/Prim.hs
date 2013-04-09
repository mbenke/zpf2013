module MyParsec2a.Prim where
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

newtype Parser a = Parser { runParser :: State -> Reply a }
                    
item = Parser item0 where
  item0 :: State -> Reply Char
  item0 [] = Error $ unexpected "EOF"
  item0 (x:xs) = Ok x xs

eofWith :: a -> Parser a
eofWith a = eof >> return a

eof = Parser eof' where
  eof' [] = Ok () []
  eof' _ = Error (expected "EOF")

char :: Char -> Parser Char
char c = (satisfy (==c)) 

satisfy p = Parser sat' where 
  sat' []    = Error (expected "EOF") -- or check (p EOF)
  sat' (a:s) = if (p a) then Ok a s else Error (unexpected $ show a) 
    
instance Monad Parser where
  return a = Parser $ \s -> Ok a s
  
  m >>= k = Parser (bind m k) where 
    bind (Parser f) k s = case f s of
      Ok a s' -> runParser (k a) s'
      Error e -> Error e

instance Functor Parser where
  fmap f p = do { x <- p; return (f x) }
  
parserZero :: Parser a
parserZero = Parser $ \s -> Error unknownError

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s -> case runParser p s of
  Error e -> runParser q s
  ok -> ok
  
(<|>) = parserPlus  

instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus
