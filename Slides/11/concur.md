% Zaawansowane programowanie funkcyjne
% Marcin Benke
% 15 maja 2013

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

# Współbieżność

~~~~ {.haskell}
import Control.Concurrent
-- forkIO :: IO() -> IO ThreadId
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO $ forever $ putChar 'A'
  forkIO $ forever $ putChar 'B'
  threadDelay 1000              -- microseconds
~~~~

~~~ 
$ ./fork
ABABABABABABABABABABABABABABABABABABABABABABABABAB
ABABABABABABABABABABABABABABABABABABABABABABABABAB
ABABABABABABABABABABABABABABABABABABABABABABABABAB
ABABABABABABABABABABABABABABABABABAB
~~~

# Synchronizacja: `MVar`

Jednoelementowy bufor/semafor:

~~~~ {.haskell}
data MVar a

newMVar  :: a -> IO (MVar a)
takeMVar ::  MVar a -> IO a 
putMVar  :: MVar a -> a -> IO ()
~~~~

`stdout` jest chronione MVar, dlatego w poprzednim  przykładzie A i B rozkładają się w miarę równo.

# Asynchroniczne I/O

~~~~ {.haskell}
import GetURL
import Control.Concurrent

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar  
  forkIO $ do 
    writeln "START 1"
    r <- getURL "http://www.wikipedia.com/wiki/Shovel"
    putMVar m1 r
    
  forkIO $ do 
    writeln "START 2"
    r <- getURL "http://www.wikipedia.com/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  writeln "1 DONE"  
  r2 <- takeMVar m2
  writeln "2 DONE"
~~~~

```
$ ./geturls1 
START 1
START 2
1 DONE
2 DONE
```
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

# Jeszcze

~~~~ {.haskell}
import Text.Printf
import qualified Data.ByteString as B

sites = ["http://www.google.com",
         "http://haskell.org",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

main = mapM (async.http) sites >>= mapM wait
 where
   http url = do
     (page, time) <- timeit $ getURL url
     printf "downloaded: %s (%d bytes, %.3fs)\n" url (B.length page) time
~~~~

~~~~
$ ./geturls
downloaded: http://www.google.com (11369 bytes, 0.008s)
downloaded: http://haskell.org (21921 bytes, 0.012s)
downloaded: http://www.wikipedia.com/wiki/Spade (69631 bytes, 0.012s)
downloaded: http://www.wikipedia.com/wiki/Shovel (82803 bytes, 0.012s)
downloaded: http://www.yahoo.com (79788 bytes, 0.020s)
~~~~

Dlaczego wyniki sa w innej kolejnosci niz zapytania?
Przecież `mapM` jest sekwencyjne?


# IORef

~~~~ {.haskell}
data IORef a
newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()
~~~~

~~~~ {.haskell}
import Data.IORef

main = do
  px <- newIORef undefined
  writeIORef px 42
  readIORef px >>= print
~~~~

```
$ ./IORef1
2
```

# Współbieżnie?

~~~~ {.haskell}
incRef :: IORef Int -> IO ()
incRef var = do { val <- readIORef var
                ; threadDelay 1000         
       	        ; writeIORef var (val+1) }

main = do
  px <- newIORef 0
  forkIO $ incRef px
  forkIO $ incRef px
  threadDelay 3000
  readIORef px >>= print
~~~~

```
$ ./IORef2
1
```

Oops.

# Z semaforem

~~~~ {.haskell}
locking :: IO a -> MVar () -> IO a
action `locking` l = lock l >> (action <* unlock l)

main = do
  gil <- newMVar ()
  let atomically a = a `locking` gil
  main2 atomically
          
main2 atomically = do
  px <- newIORef 0
  forkIO $ atomically $ incRef px 
  forkIO $ atomically $ incRef px 
  threadDelay 3000
  readIORef px >>= print  
~~~~

```
$ runghc IORef3.hs
2
```

# ...ale...

~~~~ {.haskell}
main2 atomically = do
  px <- newIORef 0
  forkIO $ atomically $ incRef px 
  forkIO $ atomicaly  $ incRef px 
  threadDelay 3000
  readIORef px >>= print
~~~~

```
$ runghc IORef4.hs
1
```

# Konta bankowe

```
void transfer( Account from, Account to, Int amount ) {
     from.withdraw( amount );
     to.deposit( amount ); }
```

w programie współbieżnym musimy synchronizować

```
from.lock(); to.lock();
from.withdraw( amount );
to.deposit( amount );
from.unlock(); to.unlock(); }
```

a nawet to nie wystarczy, może

```
if from < to
then { from.lock(); to.lock(); }
else { to.lock(); from.lock(); }
```

# Kłopoty z semaforami

* za mało semaforów

* nie te semafory - połaczenie między semaforem, a danymi, 
które chroni nie zawsze jest jasne

* za duzo semaforów - zakleszczenie, zagłodzenie,...

* semafory w złej kolejności

Potrzebne lepsze rozwiązanie

# Software Transactional Memory

```
transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount = atomically $ do 
	 deposit to amount
	 withdraw from amount 
```

* Atomicity: wyniki `atomically` są widoczne dla innych wątków jako całość

* Isolation: w trakcie `atomically act`, na działanie `act` nie mają wpływu 
  inne wątki

# GIL?

Dwa problemy:

* jeden już widzieliśmy: nie ma gwarancji izolacji

* zabijamy współbieżność (nie bez powodu nazwałem globalny semafor `gil`)

Pierwszy możemy rozwiązać przy pomocy systemu typów

~~~~ {.haskell}
atomically :: STM a -> IO a
~~~~

przy czym w `STM` nie ma dostepu do bezpośrednio  do `IORef` 
a tylko do zmiennych transakcyjnych:

~~~~ {.haskell}
data TVar
newTVar :: a -> STM (TVar a)
readTVar :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()
~~~~

**Ćwiczenie:** rozwiń ten pomysł; wypróbuj jego przepustowość

# Optymistyczna współbieżność

* Pomysł z baz danych: `atomically` tworzy lokalny log, 
* `writeTVar` pisze do logu, nie pisząc do wspólnej pamięci
* `readTVar` sprawdza najpierw log, potem ew. czyta z pamięci, 
  zapisując odczytaną wartość w logu.
* na końcu próbujemy zatwierdzić transakcję:
    * czytamy wszystkie zmienne przez nią czytane i porównujemy z logiem
    * jeśli sie zgadza, zatwierdzamy: zapisujemy to co w logu
    * wpp wycofujemy transakcję i próbujemy ją wykonac jeszcze raz (później)

Uwaga: system musi zapewnić atomowość zatwierdzania transakcji.


# launchMissiles?

Transakcja nie może miec innych efektów ubocznych niż STM

~~~~ {.haskell}
atomically $ do 
    x <- readTVar xv
    y <- readTVar yv
    if x>y then launchMissiles
           else return ()
~~~~

''Optymistycznie'' przeczytane wartości x,y moga być nieprawdziwe, 
lepiej nie odpalać rakiet...

# STM

~~~~ {.haskell}
import Control.Concurrent
import Control.Concurrent.STM

incRef :: TVar Int -> IO ()
incRef var = atomically $ do  
                val <- readTVar var
                let x = fromInteger $ delay baseDelay         
       	        writeTVar var (val+1+x) 
                
main = do
  px <- newTVarIO 0
  mapM forkIO $ replicate 20 (incRef px)
  delay (30*baseDelay) `seq` return ()
  atomically (readTVar px) >>= print
  
baseDelay :: Integer  
baseDelay = 10^7

delay :: Integer -> Integer
delay 0 = 0
delay n = delay $! n-1
~~~~

# Ćwiczenie

Zaimplementuj `withdraw`, `deposit` przy pomocy STM.


# Blokowanie: `retry`


~~~~ {.haskell}
retry :: STM a

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
   bal <- readTVar acc
   if amount > 0 && amount > bal
      then retry
      else writeTVar acc (bal - amount) 
~~~~

Gdy brak srodków zawieszamy transakcję i próbujemy później

System wie jakie zmienne transakcja czyta i moze ją wznowic po zapisie do którejś z tych zmiennych (tu: `amount`)

# Ładniej: `check`

~~~~ {.haskell}
limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
   bal <- readTVar acc
   check $ amount > 0 && amount > bal
   writeTVar acc (bal - amount) 

check :: Bool -> STM ()
check True = return ()
check False = retry
~~~~

NB

~~~~ {.haskell}
guard           :: (MonadPlus m) => Bool -> m ()
guard True      =  return ()
guard False     =  mzero
~~~~

# Wybór

Pobierz z konta A, gdy brak środków, spróbuj konta B

~~~~ {.haskell}
limitedWithdraw2 :: Account -> Account -> Int -> STM ()
limitedWithdraw2 acc1 acc2 amt =
   limitedWithdraw acc1 amt `orElse` 
   limitedWithdraw acc2 amt
~~~~

`orElse a1 a2`

* wykonaj `a1`
* gdy `a1` blokuje (`retry`), próbuje `a2`,
* gdy i to blokuje, cała transakcja blokuje.

# Równoległość przepływu danych: monada Par

Element pośredni pomiędzy `Eval` a `Concurrent`: jawne tworzenie wątków, 
ale z zachowaniem determinizmu

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

# Równoległość przepływu danych: monada Par
 
Program mozemy przedstawic jako sieć (graf) przepływu danych, 
gdzie wierzchołki reprezentują operacje, a krawędzie zależnosci.

![](dataflow-network.png "Dataflow network")

w powyższym grafie `j` potrzebuje wyników `g` oraz `h`, te zaś potrzebują wyniku `f` natomiast sa od siebie niezależne.

Graf zaleznosci nie musi być statyczny; 
może być budowany dynamicznie w zależności od potrzeb.

# Kod

~~~~ {.haskell}
network :: IVar In -> Par Out
network inp = do
 [vf,vg,vh] <- sequence [new,new,new]
 
 fork $ do x <- get inp
           put vf (f x)
  
 fork $ do x <- get vf
           put vg (g x)
 
 fork $ do x <- get vf
           put vh (h x)
 
 x <- get vg
 y <- get vh
 return (j x y)

f x = x+1
g x = x+x
h x = x*x
j = (,) 
main = print $ runNetwork 2
~~~~


```
$ ./Par1
(6,9)
```

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

# Ćwiczenie: nqueens w róznych wariantach

Przerób rozwiązanie nqueens z poprzednich zajęć (z `Eval`) na użycie `Par`.

Toż z `forkIO+MVar`

Uwaga na granularność!



# Koniec

~~~~ {.haskell}

~~~~

# Budowanie abstrakcji przy użyciu MVar -- kanały

`MVar`: jednoelementowy bufor/semafor:

~~~~ {.haskell}
newMVar  :: a -> IO (MVar a)
takeMVar ::  MVar a -> IO a 
putMVar  :: MVar a -> a -> IO ()
~~~~

`Chan`: nieograniczony bufor (kanał)

~~~~ {.haskell}
data Chan a
newChan   :: IO ( Chan a )
readChan  :: Chan a -> IO a
writeChan :: Chan a -> a -> IO ()
~~~~

# Asynchroniczne wyjątki

