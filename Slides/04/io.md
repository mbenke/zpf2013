# Wejście-wyjście w językach funkcyjnych

* Niekontrolowane efekty uboczne (C, Lisp, ML) - nie nadaje się do użycia w języku leniwym

* strumienie (Landin, Henderson)- program jest funkcją String → String
- głównie dla języków leniwych

* strumienie synchronizowane (dialogue I/O)

    system operacyjny możemy traktowac jako funkcję

    ~~~~
	[Request] -> [Response]
    ~~~~

    a program 

    ~~~~
	[Response] -> [Request]
    ~~~~

# Niekontrolowane efekty uboczne

Jak w ML:

~~~~
import Impure
-- igetchar :: () -> Char
-- iputchar :: Char -> ()

iprint = case iputchar 'a' of -- wymuszenie ewaluacji
  () -> case igetchar () of
     c -> case iputchar c of
       () -> iputchar '\n'
~~~~

**Ćwiczenie:** używając tych funkcji:

* Napisz program który wypisze ustalony napis (np. "hello" na stdout)
* Napisz program kopiujący stdin na stdout.

`igetchar/iputchar` są zaimplementowane w Code/Impure.hs

Wykonanie tego ćwiczenia powinno wyjaśnić czemu niekontrolowane efekty
uboczne są nierealne w języku leniwym...

# Dialogowe IO

~~~~ {.haskell}
type Dialogue = [Response] -> [Request]

data Response 
  = Success
  | Chr Char
  | Str String
  | StrList [String]
  | Failure IOError
  deriving(Show)  

data Request    
  = ReadChan Chan
  | AppendChan Chan String
  | ReadFile Path
  | WriteFile Path String
  deriving(Show)    
~~~~

Lista odpowiedzi jest ewaluowana leniwie; odpowiedź będzie analizowana dopiero po skonstruowaniu pierwszego żądania.

# Dialogowe IO

~~~~ {.haskell}
cat :: Dialogue
cat ~(Success : ~((Str userInput) : ~(Success : ~(r4 : _))))
= [ AppendChan stdout "enter filename\n",
    ReadChan stdin,
    AppendChan stdout name,
    ReadFile name,
    AppendChan stdout
            (case r4 of
	    	  Str contents -> contents
		  Failure ioerr -> "can’t open file")
  ] where (name : _) = lines userInput
~~~~

* Zwróćmy uwagę na leniwe dopasowanie wzorca, oznaczone przez `~`
* Widac, że dialogowe I/O jest nieco nieporęczne
* Trzeba dbać o kolejność wyliczania
* Problematyczna modularność (jak zbudować dialog z mniejszych dialogów?)

# Dialogowe I/O - ćwiczenia

Moduł Code/Dialogue2.hs implementuje dialogowe IO. Używając go

* Napisz programy z poprzedniego zadania
* Napisz program kopiujący pliki będące jego argumentami

Do uruchomienia dialogu służy funkcja `runDialogue :: Dialogue a -> IO a`

(opcjonalnie) Zaimplementuj dialogowe I/O za pomocą z funkcji z modułu Impure, ewentualnie dopisując dodatkowe "nieczyste" funkcje.


# Kontynuacyjne I/O

Dialogi wygodniej opisywać w terminach kontynuacji: co zrobić z wynikiem, a co w wypadku błedu:

~~~~ {.haskell}
type SuccCont = Dialogue
type FailCont = IOError -> Dialogue
type StrCont = String -> Dialogue

readFile :: Name -> FailCont -> StrCont -> Dialogue
readFile name fail succ ~(resp:resps) =
	 ReadFile name : case resp of
	 	  Str val -> succ val resps
		  Failure msg -> fail msg resps

appendChan :: Chan -> String -> FailCont -> SuccCont -> Dialogue
readChan :: Chan -> StrCont -> FailCont -> Dialogue
~~~~


# Przykład

~~~~ {.haskell}
catC :: Dialogue
catC = 
     appendChan stdout "enter filename\n" abort (
     readChan stdin abort                       (\userInput ->
     letE (lines userInput)                     (\(name : _) ->
     appendChan stdout name abort               (
     readFile name fail                         (\contents ->
     appendChan stdout contents abort done)))))
     where
       fail ioerr = appendChan stdout "can’t open file" abort done

abort :: FailCont
abort err resps = []
letE :: a -> (a -> b) -> b
letE x k = k x
~~~~

* Bardziej modularne
* Łatwiejsze sekwencjonowanie, ale ciągle trzeba dbać o kolejność wyliczania

# Kontynuacyjne I/O - ćwiczenia

Napisz funkcje realizujące kontynuacyjne I/O przy pomocy dialogowego I/O

~~~~ {.haskell}
type SuccCont a = a -> Dialogue
type FailCont a = IOError -> Dialogue
type Cont a = FailCont -> SuccCont a -> Dialogue

cgetchar :: Cont Char
cputchar :: Char -> Cont ()
~~~~

oraz inne potrzebne funkcje, po czym przepisz programy z poprzedniego
zadania na wersję kontynuacyjną.

# Monady

Monada to konstruktor typów `M`, z operacjami

~~~~  {.haskell}
return :: a -> M a
(>>=)  :: M a -> (a ->  M b) -> M b
~~~~

Elementami typu `M a` są obliczenia dające wynik typu `a` 
(z potencjalnymi efektami ubocznymi)

* `return x` to obliczenie czyste
* `>>=` sekwencjonuje obliczenie z jego kontynuacją, np.

    ~~~~ {.haskell}
    readChan stdin >>=   (\userInput -> ... )
    ~~~~

# Klasa Monad

~~~~ {.haskell}
class  Monad m  where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
        -- Explicit for-alls so that we know what order to
        -- give type arguments when desugaring

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail        :: String -> m a

    {-# INLINE (>>) #-}
    m >> k      = m >>= \_ -> k
    fail s      = error s
~~~~

# Przykłady monad

* Zmieniacz stanu

~~~~ {.haskell}
type ST s a = s -> (a, s)
~~~~

* Czytelnik stanu - uproszczony zmieniacz

~~~~ {.haskell}
type SR s a = s -> a
~~~~

* Pisarz stanu  (s musi być monoidem)

~~~~ {.haskell}
type SW s a = (a,s)
~~~~

* Wyjątki

~~~~ {.haskell}
data Exc e a = Exception e | OK a
~~~~

#  Przykłady monad 2


* Kontynuacje

~~~~ {.haskell}
type Cont r a = (a->r) -> r
~~~~

* Niedeterminizm

~~~~
type List a = [a]
~~~~

* Parser

~~~~ {.haskell}
type Parser a = String -> [(a,String)]
~~~~

* Identity (Wrapper)

~~~~ {.haskell}
newtype Identity a = Identity {runIdentity :: a}
~~~~

http://blog.sigfpe.com/2007/04/trivial-monad.html

# Prawa monadyki
Każda monada musi spełniać następujące prawa:

~~~~
   1. (return x) >>= k == k x
   2. m >>= return == m
   3. (m >>= f) >>= g == m >>= (\x -> (f x >>= g))
~~~~

Pierwsze dwa prawa mówią, że `return` nie ma efektów; jest elementem neutralnym dla `(>>=)`

Trzecie prawo mówi, że sekwencjonowanie obliczeń jest łączne, czyli w pewnym sensie, że 

~~~~
 (o1;o2);o3 === o1;(o2;o3)
~~~~

...i możemy je traktować jako sekwencję `o1;o2;o3`

# Prawa monadyki, inaczej

~~~~
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> (f x >>= g)

1. return >=> g     = g
2. f >=> return     = f
3. (f >=> g) >=> h  = f >=> (g >=> h)
~~~~

Ponadto każda monada powinna być instancją klasy `Functor`, oraz

~~~~
fmap f xs = xs >>= (return . f)
~~~~


# Monadyczne I/O
W wersji kontynuacyjnej mieliśmy funkcje:

~~~~ {.haskell}
readFile   :: Name -> FailCont -> StrCont  -> Dialogue
appendChan :: Chan -> String -> FailCont -> SuccCont -> Dialogue
readChan   :: Chan -> FailCont -> StrCont  -> Dialogue
~~~~

Używając monad, możemy użyć akcji IO o określonym typie wyniku:

~~~~ {.haskell}
type IO a = FailCont -> SuccCont a -> Dialogue
readFile   :: Name -> IO String
readChan   :: Chan -> IO String
appendChan :: Chan -> String -> IO ()

throw :: IOError -> IO a
catch :: IO a ->  (IOError -> IO a) -> IO a
~~~~

# Przykład

Akcje możemy sekwencjonować używając `>>=`

~~~~ {.haskell}
catM :: IO ()
catM  = appendChan stdout "enter filename\n" >>
        readChan stdin                       >>= \userInput ->
        let (name : _) = lines userInput     in
        appendChan stdout name               >>
        catch (readFile name >>= \contents ->
              appendChan stdout contents)
             (appendChan stdout "can’t open file")
~~~~

Zauważmy, że nigdzie nie polegamy na tym, że implementacja jest dialogowa/kontynuacyjna.

Możemy zatem użyć innej implementacji, np.

~~~~ {.haskell}
type IO a = World -> (a, World)
~~~~

# Notacja `do`

~~~~
do e = e

do { e1 ; stmts } = e >> do {stmts}

do { v <- e ; stmts } = e >>= \x -> do {stmts}

do { let decls; stmts } = let decls in do {stmts}
~~~~

~~~~ {.haskell}
catM :: IO ()
catM  = do 
   appendChan stdout "enter filename\n"
   userInput <- readChan stdin
   let (name : _) = lines userInput
   appendChan stdout name     
   catch (do { contents <- readFile name; appendChan stdout contents})
         (appendChan stdout "can’t open file")
~~~~

# Monadyczne I/O - ćwiczenia

1. Używając funkcji z pierwszego zadania stwórz monadę własną monadę I/O: MIO a, z operacjami

    ~~~~ {.haskell}
    mgetchar :: MIO Char
    mputchar :: Char -> MIO ()
    runMIO :: MIO () -> IO ()
    ~~~~

    można tu użyć definicji

    ~~~~ {.haskell}
    type IO a = World -> (a, World)
    ~~~~

    pierwsze ćwiczenie powinno podpowiedzieć, czym będzie `World`

2. Napisz monadę DIO implementującą monadyczne IO przy pomocy kontynuacyjnego IO. 
Przepisz programy z poprzedniego zadania tak, by używały DIO.

