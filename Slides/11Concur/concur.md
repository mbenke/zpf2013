% Advanced Functional Programming
% Marcin Benke
% May 7, 2013

# Parallelism and concurrency

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

# Concurrency

~~~~ {.haskell}
import Control.Concurrent
-- forkIO :: IO() -> IO ThreadId
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO $ forever $ putChar 'A'
  forkIO $ forever $ putChar 'B'
  threadDelay 700
~~~~

```
./fork +RTS -N2
BABABABABABABABABABABABABABABABABABABABABABBBBBBBBBBBB
```

# Synchronisation: `MVar`

One element buffer (lock):

~~~~ {.haskell}
data MVar a
newMVar  :: a -> IO (MVar a)
takeMVar ::  MVar a -> IO a 
putMVar  :: MVar a -> a -> IO ()
~~~~

`stdout` is guarded by an MVar, hence A and B in the previous example come more or less evenly.

# Asynchronous I/O

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


```
./geturls1 +RTS -N2
START 2
START 1
1 DONE
2 DONE
```

# Better

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

# More

~~~~ {.haskell}
import TimeIt
import Text.Printf
import qualified Data.ByteString as B
-- ...
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

Why are results in different order than queries? `mapM` is sequential?


# IORef

~~~~ {.haskell}
data IORef a
newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()
~~~~

~~~~ {.haskell}
incRef :: IORef Int -> IO ()
incRef var = do { val <- readIORef var
                ; threadDelay 1000         
       	        ; writeIORef var (val+1) }

main = do
  px <- newIORef 0
  incRef px
  incRef px
  readIORef px >>= print
~~~~

```
$ ./IORef1
2
```

# Concurrently?

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

# Locking

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

# ...but...

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

# IORef4.hs

``` {.haskell}
incRef :: IORef Int -> IO ()
incRef var = do { val <- readIORef var
                ; threadDelay 1000         
       	        ; writeIORef var (val+1) }

locking :: IO a -> MVar () -> IO a
action `locking` l = lock l >> (action <* unlock l)
  
atomicaly = id

main = do
  gil <- newMVar ()
  let atomically a = a `locking` gil
  main2 atomically
          
main2 atomically = do
  px <- newIORef 0
  forkIO $ atomically $ incRef px 
  forkIO $ atomicaly  $ incRef px 
  threadDelay 3000
  readIORef px >>= print

```


# Bank accounts

```
void transfer( Account from, Account to, Int amount ) {
     from.withdraw( amount );
     to.deposit( amount ); }
```

in a concurrent program we need to carefully synchronise

```
from.lock(); to.lock();
from.withdraw( amount );
to.deposit( amount );
from.unlock(); to.unlock(); }
```

even this is not good (why?), maybe

```
if from < to
then { from.lock(); to.lock(); }
else { to.lock(); from.lock(); }
```

# Problems with locking

* not enough locks

* wrong locks - the connection between a lock and data it protects is not always clear
które chroni nie zawsze jest jasne

* too many locks - deadlock, starvation

* taking locks in a wrong order

Better solutions?

# Software Transactional Memory

```
transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount = atomically $ do 
	 deposit to amount
	 withdraw from amount 
```

* Atomicity: results of `atomically` are visible to other threads as a whole

* Isolation: during `atomically act`, no interferencs from other threads

# GIL?

Two problems

* one we have seen alreadt: no isolation guarantee

* killing concurrency (I called the global lock `gil` on purpose)

The first problem can be solved using types:

~~~~ {.haskell}
atomically :: STM a -> IO a
~~~~

where `STM` has no direct access to `IORef`, only to transaction variables:

~~~~ {.haskell}
data TVar
newTVar :: a -> STM (TVar a)
readTVar :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()
~~~~

**Exercise:** develop this idea; check its throughput

# Optimistic concurrency

* An idea from databases: `atomically` writes a local log
* `writeTVar` writes only to the log, not to shared memory.
* `readTVar` checks the log first, if not found reads the memory, writing to the log.
* eventually we attempt to commit the transaction:
    * read all vars it reads, compare with the log
    * if no discrepancies, commit - write from log to memory
    * otherwise rollback and redo later

Note: the RTS must ensure atomicity of commits



# launchMissiles?

Transactions can have no side effects other than STM

~~~~ {.haskell}
atomically $ do 
    x <- readTVar xv
    y <- readTVar yv
    when (x>y) launchMissiles
~~~~

``optimistically'' read values of x,y need not be true

better not to launch missiles...


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

```
./stm1 +RTS -N2
20
```

# Exercise

Implement `withdraw`, `deposit` using STM


# Blocking: `retry`


~~~~ {.haskell}
retry :: STM a

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do
   bal <- readTVar acc
   if amount > 0 && amount > bal
      then retry
      else writeTVar acc (bal - amount) 
~~~~

When not enough funds, stop the transaction and retry later.

The system knows which variables are read and can retry 
when one of them changes (here: `amount`).


# Better: `check`

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

# Other uses of retry - window manager

Marlow, p 181

~~~~ {.haskell}
renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus    -- <1>
  loop wins                                     -- <2>
 where
  loop wins = do                                -- <3>
    render wins                                 -- <4>
    next <- atomically $ do
               wins' <- getWindows disp focus   -- <5>
               if (wins == wins')               -- <6>
                   then retry                   -- <7>
                   else return wins'            -- <8>
    loop next
~~~~

1: read the current set of windows to display

4: call render to display the
current state and then enter a transaction to read the next
state.

7: If the states are the same, then there is no need to do anything, so we call `retry`

8: If the states are different, then we return the new state, and the loop
iterates with the new state

# Choice

Withdraw from account A, when no funds try account B.

~~~~ {.haskell}
limitedWithdraw2 :: Account -> Account -> Int -> STM ()
limitedWithdraw2 acc1 acc2 amt =
   limitedWithdraw acc1 amt `orElse` 
   limitedWithdraw acc2 amt
~~~~

`orElse a1 a2`

* execute `a1`
* if `a1` blocks (`retry`), try `a2`,
* if it also blocks, the whole transaction blocks

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

