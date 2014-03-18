# Trywialny funktor

~~~~ {.haskell}
newtype W a = W a deriving Show

instance Functor W where
  -- fmap :: (a -> b) -> W a -> W b
  fmap f (W a) = W (f a)

class Pointed f where
  pure :: a -> f a
  
instance Pointed W where
  pure = W
  
a, b :: W Int
a = pure 1
b = fmap (+1) a
-- zapakowaną wartość możemy wielokrotnie zwiększać:
s = fmap (+1)
t = s(s(a))
~~~~

# Trywialna monada

~~~~ {.haskell}
f :: Int -> W Int
f x = W (x+1)
-- Jak zastosować f dwukrotnie?

bind :: (a -> W b) -> (W a -> W b)
bind f (W a) = f a

c = bind f (f 1)

instance Monad W where
  return = W
  (W x) >>= f = f x
~~~~

**Ćwiczenia**

~~~~
g :: Int -> W Int -> W Int  -- g x (W y) = W (x+y), ale bez rozpakowywania
g x wy = undefined

h :: W Int -> W Int -> W Int --h (W x) (W y) = W (x+y), bez rozpakowywania
h wx wy = undefined

-- Udowodnij, że W spełnia prawa monadyki

join :: W (W a) -> W a -- bez rozpakowywania, tylko return i bind
join wwa = undefined
~~~~

# Join

~~~~ {.haskell}
class Functor m => Monad' m where
   pure  ::  a -> m a
-- fmap  :: (a -> b) -> m a -> m b
-- fmap g . pure === pure . g

  join :: m (m a) -> m a
-- join . fmap pure === id === join . pure
-- join . fmap join === join . join
~~~~

gdzie ta ostatnia równość jest w typie `m(m(m a)) -> m a`

* zdefiniuj `bind` przy pomocy tylko `join`, `pure` i `fmap` i sprawdź, że prawa monadyki są spełnione.

# Monada stanu

~~~~ {.haskell}
type S = Int  -- przykładowo
type SM a = S -> (a,S)

smap :: (a->b) -> (SM a -> SM b)
smap f t = first f . t 

spure :: a -> SM a
spure a s = (a, s)
-- spure = (,)

sbind :: SM a -> (a -> SM b) -> SM b
sbind f k = \s -> let (a,s') = f s in k a s'

-- uncurry ($) :: (b -> c, b) -> c
sjoin :: SM (SM a) -> SM a
sjoin mma = uncurry ($) . mma
~~~~

Wykaż, że prawa monadyki są spełnione, zarówno w wersji z bind jak i z join.

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
csthen :: CS s a r -> CS s b r -> CS s b r

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
  
tick :: CSM Int r ()
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
