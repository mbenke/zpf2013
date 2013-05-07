# Równoległość a współbieżność

A *parallel* program is one that uses a multiplicity of computational
hardware (e.g. multiple processor cores) in order to perform
computation more quickly.  Different parts of the computation are
delegated to different processors that execute at the same time (in
parallel), so that results may be delivered earlier than if the
computation had been performed sequentially.

In contrast, *concurrency* is a program-structuring technique in which
there are multiple threads of control. Notionally the threads of
control execute "at the same time"; that is, the user sees their
effects interleaved. Whether they actually execute at the same time or
not is an implementation detail; a concurrent program can execute on a
single processor through interleaved execution, or on multiple
physical processors. 

--- Simon Marlow, *Parallel and Concurrent Programming in Haskell*.

# Sudoku

Przykład z dużą ilościa obliczeń: rozwiązywanie Sudoku

Każda linia pliku wejściowego zawiera instancję problemu.

Program sekwencyjny:

~~~~ {.haskell}
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    mapM_ (evaluate . solve) grids
~~~~

~~~~
$ ghc -O2 -threaded --make sudoku1.hs
$ ./sudoku1 sudoku17.1000.txt +RTS -s
  TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)
  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  Total   time    2.53s  (  2.56s elapsed)
  Alloc rate    973,110,877 bytes per MUT second
  Productivity  96.0% of total user, 94.9% of total elapsed
~~~~

# Wiele procesorów?

Poznajcie Azora: 64 rdzenie, 64GB pamięci

~~~~
$ ghc -O2 -threaded --make sudoku1.hs
$ ./sudoku1 sudoku17.1000.txt +RTS -s
  TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)
  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  Total   time    2.53s  (  2.56s elapsed)
  Productivity  96.0% of total user, 94.9% of total elapsed
~~~~

~~~~
$ ./sudoku1 sudoku17.1000.txt +RTS -s -N16
  TASKS: 18 (1 bound, 17 peak workers (17 total), using -N16)
  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  Total   time   16.84s  (  4.09s elapsed)
  Productivity  51.8% of total user, 213.1% of total elapsed
~~~~

Program działa wolniej: niepotrzebnie uruchamiamy N-1 dodatkowych wątków, które tylko przeszkadzają.

# Monada `Eval` --- strategie obliczeń

~~~~ {.haskell}
-- Control.Parallel.Strategies
data Eval a
instance Monad Eval

runEval :: Eval a
rseq :: a -> Eval a  -- "w tym wątku"
rpar :: a -> Eval a  --  "w nowym wątku"
~~~~

Wywołanie leniwej funkcji w nowym wątku ma mało sensu

Musimy sterować ilością obliczeń

# deepseq & friends

deepseq: fully evaluates the first argument, before returning the second.

~~~~ {.haskell}
seq :: a -> b -> b
-- Control.DeepSeq
class NFData a where
    rnf :: a -> ()
-- rnf should reduce its argument to normal form 
-- (that is, fully evaluate all sub-components), 
-- and then return '()'

-- Default implementation
    rnf a = a `seq` ()

deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

force ::  NFData a => a -> a
force x = deepseq x x

-- Control.Exception
-- Forces its argument to be evaluated to weak head normal form 
-- when the resultant IO action is executed.
evaluate :: a -> IO a
~~~~

# deepseq & friends

`seq` oblicza wyrażenie ``płytko'' (tylko korzeń drzewa)

`deepseq` oblicza głęboko (całe drzewo a do liści)

```
> let x = [undefined] :: [Int] in x `seq` length x
1

> let x = [undefined] :: [Int] in x `deepseq` length x
*** Exception: Prelude.undefined
```
# Program równoległy

~~~~ {.haskell}
import Control.Parallel.Strategies  -- cabal install parallel
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (force (map solve as))
       b <- rpar (force (map solve bs))
       rseq a
       rseq b
       return ()
~~~~

Tworzymy tu dwa wątki, w GHC nazywane "sparks" (to są lekkie wątki, nie wątki systemowe).


# Wyniki

~~~~
$ ghc -O2 -rtsopts -threaded --make sudoku2.hs
$ ./sudoku2 sudoku17.1000.txt +RTS -N2 -s -RTS

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)
  SPARKS: 2 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  Total   time    2.73s  (  1.77s elapsed)
  Productivity  91.1% of total user, 140.4% of total elapsed
~~~~

To już lepiej, ale ciągle nie potrafimy wykorzystac maszyny:

```
./sudoku2 sudoku17.1000.txt +RTS -N16 -s -RTS

  TASKS: 18 (1 bound, 17 peak workers (17 total), using -N16)
  SPARKS: 2 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  Total   time   15.12s  (  3.19s elapsed)
  Productivity  55.2% of total user, 261.7% of total elapsed
```

# Iskry

* Nowa "iskra" jest tworzona prz kazdym użyciu `rpar`

* Gdy tylko system ma jakąś wolną jednostkę (procesor, rdzeń, etc), przydzielamy mu iskrę z kolejki (to jest "converted").

* Jedna jednostka zawsze zajęta przez główny wątek

Tworzenie iskier mioże się nie powieść z powodu

* przepełnienia kolejki (overflow)

* wyrażenie zostało już obliczone (dud)

# Kolejka iskier

Iskry z kolejki mogą zostać 

* "skonwertowane" (converted)

* obliczone poza kolejką (fizzle)

* odśmiecone (GC)

# 

![spark lifecycle](spark-lifecycle800.png "Life cycle of a spark")

# sudoku2.hs
~~~~
  SPARKS: 2 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  Total   time    2.73s  (  1.77s elapsed)

  Productivity  91.1% of total user, 140.4% of total elapsed
~~~~

Zauważmy, że ciągle odłogiem leży "pół rdzenia".

# Threadscope

* Narzędzie do analizy wykonania programu równoległego

~~~~
$ ./sudoku2 sudoku17.1000.txt +RTS -N2 -ls
$ threadscope sudoku2.eventlog &
$ ~/.cabal/bin/threadscope sudoku2.eventlog &
~~~~

# Threadscope - sudoku2

![](sudoku2.png "sudoku2.eventlog")

# Dynamiczny podział pracy

~~~~ {.haskell}
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
~~~~

Obliczenie:

~~~~ {.haskell}
    evaluate $ force $ runEval $ parMap solve grids
~~~~

# parMap - wyniki

~~~~
$ ./sudoku3b sudoku17.1000.txt +RTS -N2 -s -RTS
  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)
  SPARKS: 1000 (1000 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  Total   time    2.84s  (  1.49s elapsed)
  Productivity  88.9% of total user, 169.6% of total elapsed
~~~~

Lepsza produktywność, poza tym łatwiej skalować na więcej rdzeni:

~~~~
sudoku2b 
-N8: Productivity  71.0% of total user, 169.2% of total elapsed
N16: Productivity  53.5% of total user, 252.6% of total elapsed

sudoku3b 
-N8: Productivity  78.5% of total user, 569.3% of total elapsed
N16: Productivity  62.8% of total user, 833.8% of total elapsed
N32: Productivity  43.5% of total user, 1112.6% of total elapsed
~~~~

# Threadscope - sudoku3

![](sudoku3.png "sudoku3.eventlog")

# Threadscope - sudoku3 -N8

![](sudoku3-N8.png "sudoku3-N8.eventlog")

# Strategie

Dodatkowy poziom abstrakcji zbudowany na monadzie `Eval`

~~~~ {.haskell}
type Strategy a = a -> Eval 
rseq :: Strategy a
rpar :: Strategy a
r0 :: Strategy a
r0 x = return x
rdeepseq :: NFData a => Strategy a
rdeepseq = rseq(deep x)

using :: a -> Strategy a -> a
x `using` s = runEval (s x)
~~~~

Zaletą takiego podejścia jest to, że "using s" można usuwać (prawie) bez zmian semantyki (program może się najwyżej stać "bardziej zdefiniowany")

# Równoległe przetwarzenie listy

~~~~ {.haskell}
parMap f xs = map f xs `using` parList rseq

-- Control.Parallel.Strategies
parList :: Strategy a -> Strategy [a]
parList strat [] = return []
parList strat (x:xs) = do
	x' <- rpar (x `using` strat)
	xs' <- parList strat xs
	return (x':xs)
~~~~

# Uwaga

Iskry są bardzo tanie, ale mimo wszystko nie należy tworzyć ich zbyt wiele

~~~~ {.haskell}
parFib n | n < 2 = n
parFib n = p `par` q `pseq` (p + q)
    where
      p = parFib $ n - 1
      q = parFib $ n - 2

main :: IO ()
main = print $ parFib 40
~~~~

~~~~
@azor:
./badfib +RTS -N2 -s -RTS
  SPARKS: 165597322 (16 converted, 14860689 overflowed, 0 dud, 
                     150628741 GC'd, 107876 fizzled)
  Total   time    7.18s  (  3.65s elapsed)
  Productivity  71.8% of total user, 141.5% of total elapsed

N60:
 SPARKS: 190193153 (61919 converted, 2556576 overflowed, 0 dud, 
                    140401503 GC'd, 47173155 fizzled)
  Total   time   65.95s  (  1.28s elapsed)
  Productivity  47.8% of total user, 2461.5% of total elapsed
~~~~

# Życie iskry 

![spark lifecycle](spark-lifecycle800.png "Life cycle of a spark")


# Lepiej

~~~~ {.haskell}
cutoff :: Int
cutoff = 20

parFib n | n < cutoff = fib n
parFib n = p `par` q `pseq` (p + q)
    where
      p = parFib $ n - 1
      q = parFib $ n - 2

fib n | n<2 = n
fib n = fib (n - 1) + fib (n - 2)
~~~~

~~~~
./parfib +RTS -N60 -s -RTS
 SPARKS: 118393 (42619 converted, 0 overflowed, 0 dud, 
                 11241 GC'd, 64533 fizzled)

  Total   time   17.91s  (  0.33s elapsed)
  Productivity  98.5% of total user, 5291.5% of total elapsed

-N60, cutoff=15
SPARKS: 1303880 (241672 converted, 0 overflowed, 0 dud, 
		 163591 GC'd, 898617 fizzled)
  Total   time   18.32s  (  0.41s elapsed)
  Productivity  98.1% of total user, 4366.7% of total elapsed
~~~~

# Threadscope

~~~~
$ ghc -O2 -threaded -eventlog --make badfib.hs
$ ./badfib +RTS -N2 -ls
$ ~/.cabal/bin/threadscope badfib.eventlog
~~~~

![threadscope:badfib](badfib.png "Threadscope")

# Threadscope

~~~~
$ ghc -O2 -threaded -eventlog --make parfib.hs
$ ./parfib +RTS -N2 -ls
$ ~/.cabal/bin/threadscope parfib.eventlog
~~~~

![threadscope:badfib](parfib.png "Threadscope")


# Ćwiczenie

Napisz funkcję rozmieszczającą n hetmanów na szachownicy n*n

* sekwencyjnie

* równolegle

~~~~ {.haskell}
type PartialSolution = [Int]
type Solution = PartialSolution
type BoardSize = Int

queens :: BoardSize -> [Solution]
queens n = iterate (concatMap (addQueen n)) [[ ]] !! n

addQueen :: BoardSize -> PartialSolution -> [PartialSolution]
addQueen n s = [x : s | x <- [1..n], safe x s 1]

safe :: Int -> PartialSolution -> Int -> Bool
safe x [] n = True
safe x (c : y) n = x /= c && x /= c + n 
       && x /= c - n && safe x y (n + 1)
~~~~

# Współbieżność

~~~~ {.haskell}
import Control.Concurrent
-- forkIO LL IO() -> IO ThreadId
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO $ forever $ putChar 'A'
  forkIO $ forever $ putChar 'B'
  threadDelay (10^6)
~~~~

# Synchronizacja: `MVar`

Jednoelementowy bufor/semafor:

~~~~ {.haskell}
data MVar a
newMVar :: a -> IO (MVar a)
takeMVar ::  MVar a -> IO a 
putMVar :: MVar a -> a -> IO ()
~~~~

`stdout` jest chronione MVar, dlatego w poprzednim  przykładzie A i B rozkładają się w miarę równo.

# Asynchroniczne I/O

~~~~ {.haskell}
import GetURL(getURL)
import Control.Concurrent

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar  
  forkIO $ do 
    r <- getURL "http://www.wikipedia.com/wiki/Shovel"
    putMVar m1 r
    
  forkIO $ do 
    r <- getURL "http://www.wikipedia.com/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  print "1 DONE"  
  r2 <- takeMVar m2
  print "2 DONE"
~~~~

# Ładniej

~~~~ {.haskell}
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyMVar
   forkIO (action >>= putMVar var)
   return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

main = do
  m1 <- async $ getURL "http://www.wikipedia.com/wiki/Shovel"
  m2 <- async $ getURL "http://www.wikipedia.com/wiki/Spade"
  wait m1
  print "1 DONE"  
  wait m2
  print "2 DONE"
~~~~

# Równoległość danych: monada Par

Element pośredni pomiędzy `Eval` a `Concurrent`: jawne tworzenie wątków, ale z zachowaniem determinizmu

~~~~ {.haskell}
newtype Par a
instance Functor Par
instance Applicative Par
instance Monad Par

runPar :: Par a -> a
fork :: Par () -> Par ()
~~~~

# Komunikacja --- IVar

~~~~ {.haskell}
data IVar a
new :: Par (IVar a)
put :: NFData a => IVar a -> a -> Par ()
get :: IVar a -> Par a
~~~~

* `new` tworzy nową , pustą zmienną

* `put` wypełnia ją wartością (mozna tylko raz)

* `get` pobiera wartość, ewentualnie czekając

# Sudoku z użyciem `Par`

~~~~ {.haskell}
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    print $ length $ filter isJust $ runPar $ do
       i1 <- new
       i2 <- new
       fork $ put i1 (map solve as)
       fork $ put i2 (map solve bs)
       as' <- get i1
       bs' <- get i2
       return (as' ++ bs')

--   Productivity  96.3% of total user, 141.2% of total elapsed
~~~~

# parMap

~~~~ {.haskell}
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
      i <- new
      fork (p >>= put i)
      return i

parMapM f as = do
	ibs <- mapM (spawn . f) as
	mapM get ibs

-- Control.Monad.Par.parMap
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    print (length (filter isJust (runPar $ parMap solve grids)))

-- Productivity  95.8% of total user, 173.1% of total elapsed
~~~~

# Koniec

~~~~ {.haskell}

~~~~
