# Exercise: unbounded channels

`MVar` represent a one-element channel. Use them to implement unbounded channels:

~~~
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))
        
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

newChan :: IO (Chan a)
-- |Build and return a new instance of Chan.

writeChan :: Chan a -> a -> IO ()
-- |Write a value to a Chan.

readChan :: Chan a -> IO a
-- |Read the next value from the Chan.
~~~

NB this is available as `Control.Concurrent.Chan` but try to avoid cheating

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

# Software Transactional Memory

```
transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount = atomically $ do 
	 deposit to amount
	 withdraw from amount 
```

**Exercise**

Implement `withdraw`, `deposit` using STM

Zaimplementuj `withdraw`, `deposit` przy pomocy STM.

# Exercise: TChan

Reimplement unbounded channels you implemented before using STM:


~~~~ {.haskell}
data TChan a = TChan (TVar (TVarList a))
                     (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
readTChan :: TChan a -> STM a
writeTChan :: TChan a -> a -> STM ()
~~~~

# Exercise: more `nqueens` variants

Rewrite `nqueens` from last week (using `Eval`) to use `Par`

Ditto with `forkIO+MVar`

Careful with granularity!

