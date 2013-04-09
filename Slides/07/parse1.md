# Kombinatory parsujące

Kod w katalogu Code/Parse1

Możemy zdefiniować typ parserów na przykład jako

~~~~ {.haskell}
newtype Parser a = Parser { runParser :: 
                       String -> [(a,String)] }
~~~~

albo, używając transformatorów monad

~~~~ {.haskell}
type Parser a = StateT [Char] (ErrorT String []) a
~~~~

# Kombinatory parsujące

oraz kombinatory (funkcje) reprezentujące elementarne parsery i sposoby łączenia parserów:

~~~~ {.haskell}
item :: Parser Char
eof :: Parser ()
(<|>) :: Parser a -> Parser a -> Parser a
satisfy :: (Char->Bool) -> Parser Char
char :: Char -> Parser Char
char x = satisfy (==x)
many, many1 :: Parser a -> Parser [a]
~~~~

~~~~
> testP eof ""
Ok () ""
> testP item ""
Error "unexpected EOF"
> testP item "p"
Ok 'p' ""
> testP (many $ char 'p') "ppq"
Ok "pp" "q"
~~~~

# Parsec

Przeanalizujmy teraz jak zbudowana jest biblioteka Parsec, 
najpierw w wersji 2, potem 3

Wersja 2 oparta jest na monadzie stanu, w pierwszym przybliżeniu:

~~~~ {.haskell}
-- Code/Parse1/MyParsec2a
newtype Parser a = Parser { runParser :: State -> Reply a }
-- Poniższe typy będą rozszerzane w kolejnych wersjach
type State = [Char]
data Reply a = Ok a State | Error ParseError  
type ParseError = String
~~~~

~~~~ {.haskell}
p3 :: Parser String
p3 = p <|> q where
  p = char 'p' >> eof >> return "p"
  q = char 'p' >> char 'q' >> eof >> return "q"
test3 = runParser p3 "pq"

*MyParsec2a> test3
Ok "q" ""
~~~~


# Podstawowe kombinatory

~~~~ {.haskell}
item :: Parser Char
item = Parser item0 where
  item0 :: State -> Reply Char
  item0 [] = Error $ unexpected "EOF"
  item0 (x:xs) = Ok x xs

eof :: Parser ()
eof = Parser eof' where
  eof' [] = Ok () []
  eof' _ = Error (expected "EOF")

char :: Char -> Parser Char
char c = (satisfy (==c)) 

satisfy :: (Char->Bool) -> Parser Char
satisfy p = Parser sat' where 
  sat' []    = Error (expected "EOF") -- or check (p EOF)
  sat' (a:s) = if (p a) then Ok a s else Error (unexpected $ show a) 
~~~~

# Sekwencjonowanie: monada

~~~~ {.haskell}
instance Monad Parser where
  return a = Parser $ \s -> Ok a s
  
  m >>= k = Parser (bind m k) where 
    bind (Parser f) k s = case f s of
      Ok a s' -> runParser (k a) s'
      Error e -> Error e

p0 = return ()
test0 = testP p0 ""

p2 = item >> item
test1 = testP p2 "" -- expect "EOF"
test2 = testP p2 "abc" -- "'b', c"
~~~~

# Zero i Plus

~~~~ {.haskell}
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
~~~~

# Lewicowa Alternatywa

Alternatywa jest ``lewicowa'': jeśli pierwszy wariant się powiódł, to
nie próbujemy drugiego - problemy jeśli gramatyka nie jest LL(1):

~~~~
*MyParsec2a> testP (many digit >> digit) "1"
Error "expected EOF"
~~~~

Mozemy sobie poradzić poświęcając wydajność za ogólność, np. 

~~~~ {.haskell}
type Parser a = StateT [Char] (ErrorT String []) a

p <|> q = mkParser $ \xs -> let 
    ps = runParser p xs
    qs = runParser q xs
    in map Right (rights ps) ++ qs
~~~~

# Ciągi

~~~~ {.haskell}
space :: Parser Char
space = satisfy isSpace

many, many1 :: Parser a -> Parser [a]
many p  = many1 p <|> return []
many1 p = do { x <- p ; xs <- many p; return (x:xs) }

skipMany :: Parser a -> Parser ()
skipMany p = many p >> return ()  -- można efektywniej
spaces p = skipMany space

~~~~

# Przykład

Za [Real World Haskell](http://book.realworldhaskell.org/read/using-parsec.html)

~~~~ {.haskell}
csvFile :: Parser [[String]]
csvFile = 
    do result <- many line
       eof
       return result
~~~~

Ale można lepiej:

~~~~ {.haskell}
csvFile = line `manyTill` eof
line = cells `endBy` eol

endBy  :: Parser a -> Parser b -> Parser a
endBy p q = do {x <- p; q; return x}

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end  = scan where
  scan = do { end; return [] }
      <|>do { x <- p; xs <- scan; return (x:xs) }
~~~~

# Jeszcze ciągi

Linia pliku CSV to ciąg komórek rozdzielonych przecinkami

~~~~ {.haskell}
cells :: Parser [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

remainingCells :: Parser [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])    
~~~~

...ale chcielibyśmy prościej:

~~~~ {.haskell}
cells = cellContent `sepBy` char ','

sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do x <- p
                         xs <- many (sep >> p)
                         return (x:xs)
~~~~

# Ciągi ze znaczącymi separatorami

Klasyczny przykład - wyrażenia arytmetyczne

~~~~ {.haskell}
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x
~~~~

# Lepsza obsługa błędów

~~~~ {.haskell}
p4 :: Parser Int
p4 = fmap digitToInt (digit)
test4a = testP p4 "7"
test4b = testP p4 "x"

-- >>> test4a
-- Ok 7 ""
-- >>> test4b
-- Error "unexpected 'x'"
~~~~

# Lepsza obsługa błędów

Chcielibyśmy, by komunikat o błędzie podawał: 

* gdzie wystąpił błąd
* czego oczekiwano...

Dla zrealizowania pierwszego postulatu, stan musi przechowywać bieżącą pozycję, np.

~~~~ {.haskell}
newtype State = State {stPos :: Pos, stInput :: String}
~~~~

* Ćwiczenie: zmodyfikuj MyParsec2a tak, aby przechowywał i raportował pozycje błędów.

# Lepsze raportowanie błędów

~~~~ {.haskell}
digit :: Parser Char
digit = satisfy isDigit <?> "digit"

-- *MyParsec2b> test4b
-- Error (Expected ["digit"])
~~~~


# Lepsze raportowanie błędów

~~~~ {.haskell}
type ParseError = Message -- lepiej [Message]

data Message = 
  UnknownError String
  | Unexpected String
  | Expected [String]
               deriving Show

p <?> expected = label p expected

label p expected = Parser $ \st -> case runParser p st of
  Ok a st' -> Ok a st'
  Error e -> Error $ addExpected expected e
  
addExpected x (Expected xs) = Expected (x:xs)
addExpected x (UnknownError _) = Expected [x]
addExpected x (Unexpected _) = Expected [x]
~~~~

# Ćwiczenie

Ćwiczenie: zmodyfikuj swoje rozwiązanie poprzedniego ćwiczenia by działało jak Parsec:

~~~~
Prelude Text.Parsec> parse digit "" ""
Left (line 1, column 1):
unexpected end of input
expecting digit
~~~~

# Usprawnianie

~~~~ {.haskell}
gen 0 = "1"
gen n = ('1':'+':'1':'-':gen (n-1))

pNum :: Parser Int
pNum = fmap digitToInt digit

pExp = pNum `chainl1` addop
addop   =   do{ char '+'; return (+) }
          <|> do{ char '-'; return (-) }

test n =  parse pExp "gen" (gen n)
~~~~

Parsec jest szybszy niz nasze kombinatory:

~~~~
parsec 3:
benchmarking gen 1e5
mean: 108.4311 ms

MyParsec2a:
benchmarking gen 1e5
mean: 138.7350 ms
~~~~

Szczegóły: pmresults.txt

# Koszt alternatywy

Alternatywa jest kosztowna:

~~~~ {.haskell}
parserPlus p q = Parser $ \s -> case runParser p s of
  Error e -> runParser q s
  ok -> ok
~~~~


Wejście dla `q` nie może zostać zwolnione zanim `p` się nie skończy -
potencjalny wyciek pamięci.

Idea: przy alternatywie `p <|> q` uzywamy `q` tylko gdy `p` zawodzi 
nie konsumując wejścia. 

Wtedy możemy zwolnić wejście dla `q`, gdy tylko `p` skonsumuje choć jeden znak.

~~~~ {.haskell}
data Consumed a = Consumed (Reply a)
                | Empty (Reply a)

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s -> case runParser p s of
  Empty (Error e) -> runParser q s
  Empty ok -> Empty ok
  consumed -> consumed
~~~~

# Implementacja

~~~~ {.haskell}
-- Code/Parse1/MyParsec2c
data Consumed a = Consumed (Reply a)
                | Empty (Reply a)

newtype Parser a = Parser { runParser :: State -> Consumed a }

satisfy p = Parser sat' where 
  sat' []    = Empty(Error (expected "EOF")) -- or check (p EOF)
  sat' (a:s) = if (p a) then Consumed(Ok a s) else 
                 Empty(Error (unexpected $ show a) )
~~~~

# Sekwencjonowanie

Clou programu:

~~~~ {.haskell}
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
~~~~

Jeżeli `p` konsumuje wejście, to `p >>= k` obliczy się do `Consumed x`, a x pozostanie nie obliczone (z uwagi na leniwość!). Operacje `Parser/runParser` zostaną wyoptymalizowane (uwaga: `newtype`)

Zatem `(p >> długieobliczenie) <|> q` może zwolnić `q` i wejście dla niego gdy tylko `p` skonsumuje pierwszy znak, nie czekając aż zakończy się `długieobliczenie`.

# Benchmark

~~~~
GHCOPTS=-O -rtsopts
pm2c +RTS -s -RTS:
benchmarking gen 100000
mean: 40.95024 ms, lb 39.81659 ms, ub 42.13325 ms
   4,815,377,360 bytes allocated in the heap
   2,136,425,484 bytes copied during GC
       6,267,672 bytes maximum residency (343 sample(s))
              21 MB total memory in use (0 MB lost due to fragmentation)
  MUT     time   11.19s  ( 12.45s elapsed)
  GC      time    4.36s  (  4.53s elapsed)

pm2a +RTS -s -RTS:
benchmarking gen 100000
mean: 141.0365 ms, lb 138.7846 ms, ub 143.5224 ms
   7,535,812,664 bytes allocated in the heap
   4,317,341,644 bytes copied during GC
      16,940,736 bytes maximum residency (301 sample(s))
              47 MB total memory in use (0 MB lost due to fragmentation)
  MUT     time   10.31s  ( 11.32s elapsed)
  GC      time    8.23s  (  8.72s elapsed)
~~~~

# Parsec3: kontynuacje

~~~~ {.haskell}
newtype Parsec a = Parsec { unParser :: forall b.
                                 State
                              -> (a -> State -> b) --  cok
                              -> (ParseError -> b) --  cerr
                              -> b
                            }
                    
item' [] cok cerr = cerr (unexpected "EOF")
item' (x:xs) cok cerr = cok x xs
item = Parsec item'

eof :: a -> Parsec a
eof a = Parsec eof' where
  eof' [] cok cerr = cok a []
  eof' _ cok cerr = cerr (expected "EOF")
~~~~

#

~~~~ {.haskell}
instance Monad Parser where
  return a = Parser (pure a) where
    pure a s cok _ = cok a s
  
  m >>= k = Parser (bind m k) where 
    bind (Parser f) k s cok cerr = f s mcok cerr where
      mcok a s = runParser (k a) s cok cerr
      mcerr = undefined

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s cok cerr -> let
    pok = cok
    perr = \e -> runParser q s cok cerr
 in runParser p s pok perr 
~~~~

# Benchmark

~~~~
GHCOPTS=-O -rtsopts
pm2c +RTS -s -RTS:
benchmarking gen 100000
mean: 40.95024 ms, lb 39.81659 ms, ub 42.13325 ms
   4,815,377,360 bytes allocated in the heap
   2,136,425,484 bytes copied during GC
       6,267,672 bytes maximum residency (343 sample(s))
              21 MB total memory in use (0 MB lost due to fragmentation)

pm3a +RTS -s -RTS:
benchmarking gen 100000
mean: 35.37689 ms, lb 34.61368 ms, ub 36.20859 ms
   4,209,433,272 bytes allocated in the heap
   2,128,069,148 bytes copied during GC
       6,234,896 bytes maximum residency (309 sample(s))
              21 MB total memory in use (0 MB lost due to fragmentation)
~~~~

# Ćwiczenie

* Ćwiczenie: połączyć pomysły 2c (Empty/Consumed) i 3a (kontynuacje) ewentualnie można jeszcze dołożyć 2b (obsługa błędów).

* Wskazówka:

~~~~ {.haskell}
newtype Parser a = Parser {unParser :: forall b .
                 State
              -> (a -> State -> ParseError -> b) -- consumed ok
              -> (ParseError -> b)               -- consumed err
              -> (a -> State -> ParseError -> b) -- empty ok
              -> (ParseError -> b)               -- empty err
              -> b
             }
~~~~
