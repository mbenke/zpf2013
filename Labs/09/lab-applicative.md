# Klasa Applicative

~~~~ {.haskell}
-- Control.Applicative
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
~~~~

Analogia z monadycznym `ap`:

~~~ {.haskell}
ap :: Monad m => m (a -> b) -> m a -> m b
~~~

Przykład:

~~~~ {.haskell}
instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _ = Nothing
  
-- >>> fmap (+1) (Just 5)
-- Just 6
-- >>> pure (+1) <*> Just 5
-- Just 6
-- >>> pure (+) <*> Just 2 <*> Just 2
-- Just 4
~~~~

# Prawa

~~~~
fmap g x = pure g <*> x
pure id <*> u = u (konsekwencja powyższego i praw fmap)
pure (.) <*> u <*> v <*> w =  u <*> v <*> w
pure f <*> pure x = pure (f x)
u <*> pure x = pure (\f -> f x) <*> u
~~~~

W  stylu aplikatywnym `fmap` zapisujemy jako `<$>`:

~~~~ {.haskell}
f <$> u = pure f <*> u
~~~~

**Ćwiczenie:** sprawdź, że powyższe prawa zachodzą dla podanej instancji dla `Maybe`.

# Data.Foldable

~~~~ {.haskell}
import Prelude hiding (foldl, foldr, foldl1, foldr1)
import qualified Prelude(foldl, foldr, foldl1, foldr1)
import Data.Monoid

-- Prelude.foldr :: (a -> b -> b) -> b -> [a] -> b

class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
  foldMap :: Monoid m => (a -> m) -> t a -> m

  -- | Combine the elements of a structure using a monoid.
  fold :: Monoid m => t m -> m
  fold = foldMap id

  -- foldl, foldr',foldl',foldl1,foldr1,...      
~~~~

*Ćwiczenie:* 

* napisz kilka  instancji `Foldable`
* wyraź `foldr` przez `foldMap` i vice versa
* napisz funkcję 

~~~~ {.haskell}
toList :: Foldable t => t a -> [a]
~~~~


# Funkcyjny iterator

`dist` może być uzyte w połączeniu z `map` np

~~~~ {.haskell}
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap m as = dist (map m as)
~~~~

...ale tu przechodzimy listę dwa razy, a wystarczy raz:

~~~~ {.haskell}
traverseL :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseL f []     = pure []
traverseL f (x:xs) = (:) <$> f x <*> traverseL f xs
~~~~

Mozna to uogólnić na dowolne struktury iterowalne:

~~~~ {.haskell}
class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist = traverse id
~~~~

**Ćwiczenie:** napisz instancje `Traversable` dla drzew

# Idiomy a monady

Każda monada jest funktorem aplikatywnym, np.

~~~~ {.haskell}
instance Applicative Maybe where
  pure = return
  (<*>) = ap

ap mf mx = mf >>= \f -> m x >>= \x -> return (f x)
~~~~

Natomiast w ogólności nie na odwrót, np. nasza instancja dla list

~~~~ {.haskell}
instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
~~~~

czy da się zdefiniować `>>=` tak by `ap` odpowiadało `<*>` ?

*Ćwiczenie:* sprawdź, że prawa dla idiomów wynikają z praw dla monad.

# Idiomy a monady

Strukturze monadycznej dla list odpowiada inna instancja Applicative dla list, gdzie listę funkcji aplikujemy do listy argumentów metodą "każdy z każdym":

~~~~ {.haskell}
instance Applicative [] where
  pure = (:[])
  fs <*> xs = concat $ for fs (for xs)
  
for = flip map
~~~~

**Ćwiczenie:** wykaż poprawność powyższej definicji

**Ćwiczenie:** napisz dwie instancje Applicative dla State.

# Parsery

** Ćwiczenie: ** napisz parser dla wyrażeń arytmetycznych, uzywając tylko idiomów (bez `do` i bez `>>=`)
