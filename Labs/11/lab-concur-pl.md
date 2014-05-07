
UWAGA: to jest stara wersja, aktualna wersja zadań w lab-concur-en.md

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

# Software Transactional Memory

```
transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount = atomically $ do 
	 deposit to amount
	 withdraw from amount 
```

**Ćwiczenie**

Zaimplementuj `withdraw`, `deposit` przy pomocy STM.

# Ćwiczenie: nqueens w róznych wariantach

Przerób rozwiązanie nqueens z poprzednich zajęć (z `Eval`) na użycie `Par`.

Toż z `forkIO+MVar`

Uwaga na granularność!
