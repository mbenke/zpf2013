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
