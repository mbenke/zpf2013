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

# Składanie idiomów

~~~~ {.haskell}
-- g :: * -> *,  f :: * -> * => g :. f :: * -> *
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  pure  = O . pure . pure
  O gs <*> O xs = -- O (| (<*>) gs xs |) 
                  O ( (<*>) <$> gs <*> xs)
~~~~

**Ćwiczenie:** zdefiniować

~~~~ {.haskell}
instance (Functor g, Functor f) => Functor (g :. f) where ...
~~~~

i sprawdzić, że złożenie funktorów aplikatywnych spełnia prawa dla funktora aplikatywnego.

# Parsery

** Ćwiczenie: ** napisz parser dla wyrażeń arytmetycznych, uzywając tylko idiomów (bez `do` i bez `>>=`)
