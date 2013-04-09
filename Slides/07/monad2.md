
# Continuation Passing Style (CPS)

Z kontynuacjami zetknęliśmy się już przy okazji I/O:

~~~~ {.haskell}
readFile :: Name -> FailCont -> StrCont -> Dialogue
~~~~

Dowolną funkcję można przerobic na styl kontynuacyjny, np.

~~~~ {.haskell}
add :: Int -> Int -> Int
add x y = x + y

add_cps :: Int -> Int -> (Int -> r) -> r
add_cps x y k =  k (x+y)

type Cont r a = (a -> r) -> r
add_cps' :: Int -> Int -> Cont r Int
add_cps' = add_cps
~~~~

~~~~
*Main> add_cps 2 2 id
4
*Main> add_cps 2 2 show
"4"
~~~~

# CPS (2)

~~~~ {.haskell}
square_cps :: Int -> (Int -> r) -> r
square_cps x k = k (square x)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y k =
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_cps x_squared y_squared $ \sum_of_squares ->
 k sum_of_squares
~~~~

~~~~
*Main> pythagoras_cps 3 4 id
25
~~~~

# Na marginesie: transformacja CPS

`T` zamienia term typu `a` na `((a->r)->r)`

~~~~

T(x) = \k -> k x
T(\x -> M) = \k.k(\x -> T(M))
T(MN) = \k -> T(M)(\m -> 
              T(N)(\n -> 
              mnk))
~~~~

# Monada kontynuacji

~~~~ {.haskell}
type Cont r a = (a -> r) -> r
-- Związek z logiką:  Cont a ∼ (a → ⊥) → ⊥ = ¬¬a

contra :: (a->b) -> (b->r) -> (a->r)
contra f g = g . f

cmap :: (a -> b) -> Cont r a -> Cont r b
--   :: (a -> b) -> ((a -> r) -> r) ->  (b -> r) -> r
cmap f m = \c -> m $ c . f -- \c -> m (contra f c)

cpure :: a -> Cont r a
cpure = flip ($) -- \a c -> c a

cbind :: Cont r a -> (a -> Cont r b) -> Cont r b
-- ((a->r)->r)) -> (a -> (b->r)->r)
cbind m k = \c -> m (\a -> k a c)
~~~~

Jak zwykle w bibliotece jest to zapakowane w newtype, ale mamy funkcje

~~~~ {.haskell}
cont :: ((a->r)->r) -> Cont r a
runCont :: Cont r a -> (a->r)->r
~~~~

# Monada kontynuacji (2)

~~~~ {.haskell}
import Control.Monad.Cont

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (x+y)

square_cont :: Int -> Cont r Int
square_cont x = return (x*x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y =
    do x_squared <- square_cont x
       y_squared <- square_cont y
       sum_of_squares <- add_cont x_squared y_squared
       return sum_of_squares
~~~~

~~~~
*Main> runCont (pythagoras_cont 3 4) id
25
~~~~




# Kontynuacje

~~~~ {.haskell}
import Control.Monad.Cont

ex1 :: Cont r Int
ex1 = do
  a <- return 1
  b <- return 10
  return (a+b)
  
-- test :: (forall r. (Show r) => Cont r Int) -> String 
test ex = runCont ex show
~~~~

~~~~
> test ex1
"11"
~~~~

~~~~ {.haskell}
-- cont :: ((a->r)->r) -> Cont r a
ex2 :: Cont r Int
ex2 = do
  a <- return 1
  b <- cont (\c -> c 10)
  return (a+b)
~~~~

~~~~
> test ex2
"11"
~~~~

# Brak wyniku - wyjątki

~~~~ {.haskell}
ex3 = do
   a <- return 1
   b <- cont (\c -> "escape")
   return $ a+b
~~~~

~~~~
> test ex3
"escape"
~~~~

...czyli mamy wyjątki


~~~~ {.haskell}
escape :: r -> Cont r a
escape r = cont (const r)
~~~~

~~~~ {.haskell}
ex3e = do
   a <- return 1
   b <- escape "escape"
   return $ a+b
~~~~


# Wiele wyników

~~~~ {.haskell}
ex4 = do
   a <- return 1
   b <- cont (\c -> c 10 ++ c 20)
   return $ a+b
~~~~

~~~~
> test ex4
"1121"
~~~~

Hmm, to prawie jak monada list:

~~~~ {.haskell}
test5 = do 
  a <- return 1
  b <- [10, 20]
  return $ a+b   
~~~~

~~~~
> test5
[11,21]
~~~~

# Wiele wyników (2)

~~~~ {.haskell}
ex6 = do
  a <- return 1
  b <- Cont (\c -> c 10 ++ c 20)
  return $ a+b

test6 = runCont ex6 (\x -> [x])
~~~~

~~~~
> test6
[11,21]
~~~~

Albo inaczej:


~~~~ {.haskell}
ex7 = do
   a <- return 1
   b <- cont (\c -> concat [c 10, c 20])
   return $ a+b

test7 = runCont ex7 (\x -> [x])

ex8 = do
  a <- return 1
  b <- cont (\c -> [10,20] >>= c)
  return $ a+b

test8 = runCont ex8 return
~~~~

# Ćwiczenie - kontynuacje a stan

~~~~
Stan:
 s -> (a,s)

CPS:   s -> ((a,s) -> r) -> r
curry: s -> (a -> s -> r) -> r
flip:  (a -> s -> r) -> s -> r
~~~~

Zdefiniuj

~~~~ {.haskell}
type CS s a r = (a -> s -> r) -> s -> r
-- Odpowiednik Functor
csmap :: (a->b) -> CS s a r -> CS s b r

-- Odpowiednik Monad
cspure :: a -> CS s a r
csbind :: CS s a r -> (a -> CS s b r) -> CS s b r

foo = csmap (+1) (cspure 41)
~~~~

i sprawdź, że działa:

~~~~
>>> foo (\a s -> show a) 17
"42"
~~~~

# Ćwiczenie - kontynuacje a stan (2)

~~~~ {.haskell}
-- Odpowiednik MonadState
-- get :: MonadState s m => m s
csget :: CS s s r

-- put :: MonadState s m => s -> m ()
csput :: s -> CS s () r

csmodify :: (s->s) -> CS s () r
csmodify t = csget `csbind` (\s -> csput (t s))

cstick :: CS Int () r
cstick = csmodify (+1)

bar :: CS Int Int r
bar = csput 40 `csthen` cstick `csthen` cstick `csthen` csget
~~~~

...i sprawdź, że działa:

~~~~
*Main> bar const 0
42
~~~~

Uwaga:

* nie importuj Control.Monad.State
* nie zaglądaj do jego źródeł

# Ćwiczenie - kontynuacje a stan (3)

Zdefiniuj monadę stanu przy pomocy Cont:

~~~~ {.haskell}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, MultiParamTypeClasses #-}
import Control.Monad.Cont
-- Uwaga: nie importujemy Control.Monad.State
class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do { s <- get; put (f s) }

-- w miejsce X wstaw wyrażenie używające Cont, s, r
type CSM s r a = X a 

instance MonadState s X where
...
-- Wskazówka: cont :: ((a->s->r)->s -> r) -> Cont (s->r) a
  
tick :: CSM Int r Int
tick = modify (+1)

baz :: CSM Int r Int
baz = do { put 40; tick; tick; get }
~~~~

...i sprawdź, że działa:

~~~~
*Main> runCont baz const 0
42
~~~~

# Ćwiczenie: kontynuacje a błędy

~~~~ {.haskell}
{-
Error: (a + e)
CPS: ((a + e) -> r) -> r
de Morgan: (a ->r,e -> r) -> r
curry: (a->r) -> (e->r) -> r
-}

type CE e a r = (e->r) -> (a->r) -> r
cemap :: (a->b) -> CE e a r -> CE e b r
cepure :: a -> CE e a r
cebind :: CE e a r -> (a -> CE e b r) -> CE e b r

throwCE :: e -> CE e a r
catchCE :: CE e a r -> (e -> CE e a r) -> CE e a r

uncurryCE :: ((e->r) -> (a->r) -> r) -> ((e ->r,a -> r) -> r)
-- Prelude.either :: (e->r) -> (a->r) -> Either e a ->r
-- ~ ((e->r), (a->r)) -> Either e a ->r
coeither :: (Either e a -> r) -> (e ->r, a -> r)
morgan1 :: ((e ->r,a -> r) -> r) -> (Either e a -> r) -> r
morgan2 :: ((Either e a -> r) -> r) -> (e -> r, a -> r) -> r

-- te funkcje ustanawiaja izomorfizm
iso1 :: ((e->r) -> (a->r) -> r) -> ((Either e a) -> r) ->r
iso2 :: ((Either e a -> r) -> r) -> (e -> r) -> (a -> r) -> r


newtype CEM e r a = CEM { runCEM :: Cont r (Either e a) }
toCEM :: CE e a r -> CEM e r a
fromCEM :: CEM e r a -> CE e a r

instance Monad (CEM e r) where ...  
instance (Error e) => MonadError e (CEM e r) where...
~~~~ 

# Monada wolna

Dla każdego funktora możemy zdefiniować monadę:

~~~~ {.haskell}
data Free f a = Pure a | In (f (Free f a))

instance Functor f => Functor (Free f) where
   fmap f (Pure a) = Pure (f a)
   fmap f (In as) = In (fmap (fmap f) as)
 
instance Functor f => Monad (Free f) where
   return = Pure
   Pure a >>= f = f a -- pierwsze prawo
   In  as >>= f = In (fmap (>>= f) as)
~~~~

W literaturze `In` nazywa się też `Free`:

~~~~ {.haskell}
data Free f a = Pure a | Free (f (Free f a))
~~~~

# Przykład

~~~~ {.haskell}
import Control.Monad.Free
import System.Exit hiding (ExitSuccess)

data TeletypeF x
  = PutStrLn String x
  | GetLine (String -> x)
  | ExitSuccess

instance Functor TeletypeF where
    fmap f (PutStrLn str x) = PutStrLn str (f x)
    fmap f (GetLine      k) = GetLine (f . k)
    fmap f  ExitSuccess     = ExitSuccess
~~~~

# Przykład c.d.

~~~~ {.haskell}
type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

exitSuccess' :: Teletype r
exitSuccess' = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r) = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine  f    )) = getLine      >>= run . f
run (Free  ExitSuccess    ) = exitSuccess

echo :: Teletype ()
echo = do str <- getLine'
          putStrLn' str
          exitSuccess'
          putStrLn' "Finished"

main = run echo
~~~~
