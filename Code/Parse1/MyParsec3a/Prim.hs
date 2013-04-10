{-# LANGUAGE Rank2Types #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module MyParsec3a.Prim where
import Control.Monad
infixr 1 <|>

type ParseError = String
unexpected :: String -> ParseError
unexpected s = "unexpected " ++ s

expected :: String -> ParseError
expected s = "expected " ++ s

unknownError :: ParseError
unknownError = "Unknown error"

data Reply a = Ok a State | Error ParseError  
  deriving Show

type State = [Char]


newtype Parser a = Parser { runParser :: forall b.
                                 State
                              -> (a -> State -> b) --  cok
                              -> (ParseError -> b) --  cerr
                              -> b
                            }
                    
item' [] cok cerr = cerr (unexpected "EOF")
item' (x:xs) cok cerr = cok x xs
item = Parser item'

eof :: Parser ()
eof = Parser eof' where
  eof' [] cok cerr = cok () []
  eof' _ cok cerr = cerr (expected "EOF")

char :: Char -> Parser Char
char c = (satisfy (==c)) -- FIXME

satisfy p = Parser satisfy' where 
  satisfy' s cok cerr = item' s csat cerr where
    csat a s =  if (p a) then cok a s else cerr (unexpected $ show a)
    
instance Monad Parser where
  return a = Parser (pure a) where
    pure a s cok _ = cok a s
  
  m >>= k = Parser (bind m k) where 
    bind (Parser f) k s cok cerr = f s mcok mcerr where
      mcok a s' = runParser (k a) s' cok cerr
      mcerr = cerr

instance Functor Parser where
  fmap f p = do { x <- p; return (f x) }
  
parserZero :: Parser a
parserZero = Parser $ \s cok cerr -> cerr unknownError

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s cok cerr -> let
    pok = cok
    perr = \e -> runParser q s cok cerr
 in runParser p s pok perr 

(<|>) = parserPlus  

instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus
