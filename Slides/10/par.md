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
  Task  0 (worker) :    0.00s    (  0.00s)       0.00s    (  0.00s)
  Task  1 (worker) :    0.00s    (  2.37s)       0.00s    (  0.00s)
  Task  2 (bound)  :    2.32s    (  2.32s)       0.05s    (  0.05s)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    2.32s  (  2.32s elapsed)
  GC      time    0.05s  (  0.05s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    2.37s  (  2.37s elapsed)
~~~~

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

~~~~ {.haskell}
seq :: a -> b -> b
-- Control.DeepSeq
class NFData a where
    rnf :: a -> ()
    rnf a = a `seq` ()
deepseq :: NFData a => a -> b -> b

-- Control.Exception
-- Forces its argument to be evaluated to weak head normal form 
-- when the resultant IO action is executed.
evaluate :: a -> IO a
~~~~

# Program równoległy

~~~~ {.haskell}
import Control.Parallel.Strategies  -- cabal install parallel
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (deep (map solve as))
       b <- rpar (deep (map solve bs))
       rseq a
       rseq b
       return ()
~~~~

Tworzymy tu dwa wątki, w GHC nazywane "sparks" (to są lekkie wątki, nie wątki systemowe).


# Wyniki

~~~~
$ ghc -O2 -rtsopts -threaded --make sudoku2.hs
$ ./sudoku2 sudoku17.1000.txt +RTS -N2 -s -RTS

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    2.08s    (  1.27s)       0.38s    (  0.41s)
  Task  1 (worker) :    2.43s    (  1.64s)       0.06s    (  0.07s)
  Task  2 (bound)  :    2.44s    (  1.66s)       0.04s    (  0.05s)
  Task  3 (worker) :    2.49s    (  1.70s)       0.00s    (  0.00s)

  SPARKS: 2 (1 converted, 0 dud, 0 GC'd, 1 fizzled)

  Total   time    2.49s  (  1.70s elapsed)

  Alloc rate    478,082,040 bytes per MUT second

  Productivity  97.6% of total user, 142.7% of total elapsed
~~~~

# Iskry

* Nowa "iskra" jest tworzona prz kazdym użyciu rpar

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

![](spark-lifecycle.png "Life cycle of a spark")

# sudoku2.hs
~~~~
  SPARKS: 2 (1 converted, 0 dud, 0 GC'd, 1 fizzled)

  Total   time    2.49s  (  1.70s elapsed)

  Productivity  97.6% of total user, 142.7% of total elapsed
~~~~

Zauważmy, że ciągle odłogiem leży "pół rdzenia".

Threadscope

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

~~~~
  SPARKS: 1000 (995 converted, 0 dud, 0 GC'd, 5 fizzled)

  Total   time    2.20s  (  1.22s elapsed)

  Productivity  94.2% of total user, 169.0% of total elapsed
~~~~

Lepsza produktywność, poza tym łatwiej skalować na więcej rdzeni:

~~~~
./sudoku2b sudoku17.1000.txt +RTS -N8 -s
  Productivity  58.2% of total user, 218.2% of total elapsed

[ben@students Marlow]$ ./sudoku3b sudoku17.1000.txt +RTS -N8 -s
 Productivity  63.6% of total user, 487.4% of total elapsed
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
$ ./badfib +RTS -N2 -s -RTS

  3,321,319,864 bytes allocated in the heap
      1,173,852 bytes copied during GC
  Parallel GC work balance: 1.75 (277333 / 158455, ideal 2)

  SPARKS: 166058569 (222 converted, 113019108 overflowed, 0 dud, 
                     51863340 GC'd, 1175899 fizzled)
  Total   time    5.49s  (  2.75s elapsed)
  Productivity  90.2% of total user, 180.0% of total elapsed
~~~~

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
         911,348 bytes allocated in the heap
              20 bytes copied during GC

SPARKS: 29892 (21 converted, 5065 overflowed, 0 dud, 18 GC'd, 24788 fizzled)

Total   time    1.73s  (  0.87s elapsed)
Productivity 100.0% of total user, 198.8% of total elapsed
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
