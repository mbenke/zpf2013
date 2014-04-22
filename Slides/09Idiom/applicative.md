# Przypomnienie - Funktory

Funktor to operacja `T :: * -> *` na typach  
wraz z operacją `fmap` na funkcjach 

~~~~ {.haskell}
fmap :: (a -> b) -> (T a -> T b) 
~~~~

zachowującą strukturę składania funkcji, czyli

~~~~
fmap id = id
fmap (f . g) = fmap f . fmap g
~~~~

# Motywacja - sekwencje

Przypomnijmy sobie parser dla cyfr:

~~~~ {.haskell}
pNum :: Parser Int
pNum = fmap digitToInt digit
~~~~

co jest krótszym zapisem

~~~~ {.haskell}
pNum = do
     d <- digit
     return $ digitToInt d
~~~~

Chcielibyśmy móc podobnie uprościć schemat

~~~ {.haskell}
do x1 <- m1
   x2 <- m2
   return $ f x1 x2
~~~

# Motywacja - sekwencje

Przypomnijmy sobie funkcję `sequence`:

~~~~ {.haskell}
sequence1 :: Monad m => [m a] -> m [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)
~~~~

która łączy listę akcji zbierając ich wyniki w liste.

Możemy ją zapisac prościej uzywając "monadycznej" aplikacji, `ap`:

~~~~ {.haskell}
sequence2 (c:cs) = return (:) `ap` c `ap` sequence cs

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return $ f x
~~~~

# Motywacja - transponowanie macierzy

Macierz jako lista wierszy (równej długości)

~~~
> transpose [[1,2],[3,4],[5,6]]
[[1,3,5],[2,4,6]]

Pomysł:

 [x1, x2, ... xn]     [x1 |                ]
 [--------------]     [x2 |                ]
 [              ]  -> [ . |  transpose xss ]
 [      xss     ]     [ . |                ]
 [              ]     [xn |                ]
~~~

~~~ {.haskell}
transpose1 :: [[a]] -> [[a]]
transpose1 [xs] = [[x] | x <- xs]
transpose1 (xs:xss) = consAll xs (transpose1 xss) 

consAll :: [a] -> [[a]] -> [[a]]
consAll (y:ys) (zs:zss) = (y:zs):consAll ys zss
consAll _      _        = []
~~~

# Związek z `zip`

~~~~ {.haskell}
consAll :: [a] -> [[a]] -> [[a]]
consAll (y:ys) (zs:zss) = (y:zs):consAll ys zss
consAll _      _        = []

zip :: [a] -> [b] -> [(a, b)]
zip  (y:ys) (z:zs) = (y,z):zip ys zs
zip  _      _      = []

-- Uogólnienie zip
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (y:ys) (z:zs) = f y z:zipWith f ys zs
zipWith _  _      _      = []

zip = zipWith (,)
consAll = zipWith (:)
~~~~

NB `zip [1,2,3] (repeat 0) = [(1,0),(2,0),(3,0)]`

# Jeszcze raz transpose

Obserwacja:

~~~~ {.haskell}
> zipWith (:) [1,2,3] (repeat [])
[[1],[2],[3]]
~~~~

Czyli można

~~~~ {.haskell}
transpose2 :: [[a]] -> [[a]]
transpose2 (xs:xss) = zipWith (:) xs (transpose2 xss)
transpose2 [] = repeat []
~~~~

kłopoty z transpozycja pustej macierzy, ale:

~~~
> transpose $ transpose []
[]
> transpose [[]]
[]
~~~

# Motywacja - `zipWith`

~~~~ {.haskell}
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- zipWith_n :: (a1 -> ... -> an -> b) -> [a1] -> ... -> [an] -> [b]
-- zipWith_n f as1 ... asn = repeat f `zap` as1 `zap` ... `zap` asn
-- zap  ~ 'zippy ap'

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = repeat (:) `zap` xs `zap` transpose xss
-- zipWith (:) xs (transpose xss) == 

zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []
~~~~

Uwaga: zap ≠ ap

~~~
*Main> ap [(+1)] [1,2]
[2,3]
*Main> zap [(+1)] [1,2]
[2]
~~~

# Motywacja - interpreter

~~~~ {.haskell}
data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

eval1 :: Exp v -> Env v -> Int
eval1 (Var x) env = fetch x env
eval1 (Val i) env = i
eval1 (Add p q) env = eval1 p env + eval1 q env

eval2 (Var x) = fetch x
eval2 (Val i) = const i
eval2 (Add p q) = const (+) `apply` eval2 p 
                            `apply` eval2 q

apply :: (env -> a -> b) -> (env -> a) -> (env -> b)
apply ef es env = (ef env) (es env)
~~~~

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

# Przykłady idiomatycznie

~~~~ {.haskell}
-- sequence2 (c:cs) = return (:) `ap` c `ap` sequence2 cs
-- sequence3 (c:cs) = pure   (:)  <*> c  <*> sequence3 cs
sequence3 (c:cs) = (:) <$> c <*> sequence3 cs

instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
  
transpose3 :: [[a]] -> [[a]]
transpose3 [] = pure []
transpose3 (xs :xss) = (:) <$> xs <*> transpose3 xss
-- return (:) `ap` xs `ap` transpose4 xss ??

instance Applicative ((->) env) where
  pure = const
  ef <*> es = \env -> (ef env) (es env)

eval3 (Var x) = fetch x
eval3 (Val i) = pure i
eval3 (Add p q) = pure (+) <*> eval3 p <*> eval3 q
~~~~

# sequence = transpose ?

~~~~ {.haskell}
sequence3 [] = pure []
sequence3 (c:cs) = (:) <$> c <*> sequence3 cs
  
transpose3 [] = pure []
transpose3 (xs :xss) = (:) <$> xs <*> transpose3 xss
~~~~

To jest ta sama funkcja:

~~~~ {.haskell}
dist :: Applicative f => [f a] -> f [a]
dist []     = pure []
dist (x:xs) = (:) <$> x <*> dist xs
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
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist = traverse id
  traverse f = dist . fmap f
~~~~

NB to jest definicja z oryginalnej pracy - `Data.Traversable` używa innej definicji

**Ćwiczenie:** napisz instancje `Traversable` dla drzew

# Nawiasy idiomatyczne (idiom brackets)

Conor McBride zaproponował specjalną notację idiomatyczną:

~~~~ 
 (|f a1 .. an|) = pure f <*> a1 <*> .. <*> an
sequence4 (c:cs) = (| (:) c (sequence4 cs) |)
eval4  (Add p q) = (| (+) (eval3 p) (eval3 q) |)
~~~~

nie weszła ona do standardu Haskella, choć jest dostępna w SHE.

https://personal.cis.strath.ac.uk/~conor/pub/she/

# Nawiasy idiomatyczne (idiom brackets)

Przy pomocy klas możemy udostępnić podobną, choć brzydszą notację:

~~~~ {.haskell}
sequence4 (c:cs) = iI (:) c (sequence4 cs) Ii
eval4  (Add p q) = iI (+) (eval3 p) (eval3 q) Ii

class Applicative i => Idiomatic i f g | g -> f i where
   idiomatic :: i f -> g
 
iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure
 
data Ii  =  Ii
 
instance Applicative i    => Idiomatic i x (Ii -> i x) where
  idiomatic xi Ii     = xi
 
instance Idiomatic i f g  => Idiomatic i (s -> f) (i s -> g) where
  idiomatic sfi si    = idiomatic (sfi <*> si)
~~~~

# Control.Applicative.QQ.Idiom

Innym rozwiązaniem jest użycie Template Haskell:

~~~~
-- package applicative-quoters
 [i| subtract [1,2,3] [10,20,30] |]
 -> pure subtract <*> [1,2,3] <*> [10,20,30]
 -> [9,19,29,8,18,28,7,17,27]
~~~~

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

# Idiomy a monady

W ogólności sekwencjonowanie monadyczne jest  silniejsze od idiomatycznego:

~~~~ {.haskell}
mif c t e = do { b <- c; if b then t else e }

aif fc ft fe = cond <$> fc <*> ft <*> fe where
  cond c t e =if c then t else e

main = do
  putStrLn "Monad:"
  mif (return True) (putStrLn "True") (putStrLn "False")
  putStrLn "Idiom:"
  aif (pure True) (putStrLn "True") (putStrLn "False")
~~~~

~~~~
Monad:
True
Idiom:
True
False
~~~~

# Monoid

~~~~ {.haskell}
-- Data.Monoid
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

(<>) :: Monoid a => a -> a -> a
(<>) = mappend
~~~~

Monoid, aplikatywnie:

~~~~ {.haskell}
-- typ fantomowy: nie używa swojego argumentu
newtype Accy o a = Acc{acc::o}

instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  Acc o1 <*> Acc o2 = Acc (o1 <> o2)
~~~~

nie jest monadyczne, bo jak zdefiniować

~~~~ {.haskell}
(>>=) :: Accy o a -> (a->Accy o b) -> Accy o b
~~~~

# Akumulowanie błędów

~~~~ {.haskell}
data Except err a = Ok a | Failed err

instance Monoid err => Applicative (Except err) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Failed err = Failed err
  Failed err <*> Ok _ = Failed err
  Failed e1 <*> Failed e2 = Failed (e1 <> e2)
~~~~

trudno zrobić analog monadyczny (jest monada `Writer`, ale to nie to samo)


# Przykład

~~~~ {.haskell}
-- condMap p f xs | all p xs = map f xs
--                | otherwise = blad

condMap1 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap1 p f (x:xs) 
  | p x = do ys <- condMap1 p f xs
             return $ f x:ys
  | otherwise = Left x                 
condMap1 p f [] = return []

-- > condMap1 even (+1) [2,4]
-- Right [3,5]
-- > condMap1 even (+1) [2,3,4]
-- Left 3
~~~~

Używając monad możemy raportować pierwszy bład, ale gdybyśmy chcieli wszystkie?

# condMap monadycznie

~~~~ {.haskell}
condMap3 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap3 p f (x:xs) = do
  y  <- f <$> check p x
  ys <- condMap3 p f xs
  return (y:ys)
  where check p x = if p x then Right x else Left x
condMap3 p f [] = return []

condMap4 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap4 p f (x:xs) = (:) <$> ay <*> ays  where
  ay  = f <$> check p x
  ays = condMap4 p f xs
  check p x = if p x then Right x else Left x
condMap4 p f [] = pure []
~~~~

# condMap idiomatycznie

Możemy zapisać condMap idiomatycznie uzywając ``monadycznej'' instancji Applicative dla Either oraz praw

~~~~ {.haskell}
-- f  $  g  $  x === f . g  $ x    
-- f <$> g <$> x === f . g <$> x
~~~~

~~~~ {.haskell}
condMap5 :: (a -> Bool) -> (a->b) -> [a] -> Either a [b]
condMap5 p f (x:xs) = (:) . f <$> (check p x) <*> condMap5 p f xs 
  where check p x = if p x then Right x else Left x
condMap5 p f [] = pure []
~~~~

# condMap idiomatycznie

Teraz wystarczy drobna zmiana

~~~~ {.haskell}
condMap6 :: (a -> Bool) -> (a->b) -> [a] -> Except [a] [b]
condMap6 p f (x:xs) = (:) . f <$> (check p x) <*> condMap6 p f xs 
  where check p x = if p x then Ok x else Failed [x]
condMap6 p f [] = pure []

condEven :: [Int] -> Except [Int] [Int]
condEven = condMap6 even (+1)

-- > condEven [1..5]
-- Failed [1,3,5]

-- > condEven [2,4,6]
-- Ok [3,5,7]
~~~~

# Składanie funktorów

~~~~ {.haskell}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

gfmap :: forall f g a b. (Functor g, Functor f) 
         => (a -> b) -> g(f(a)) -> g(f(b))
gfmap fun a = mapG (mapF fun) a where
  mapF :: (a -> b) -> f a -> f b
  mapG :: (f a -> f b) -> g (f a) -> g (f b)
  mapF = fmap
  mapG = fmap 
~~~~

~~~~ {.haskell}
-- g :: * -> *,  f :: * -> * => g :. f :: * -> *
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Functor g, Functor f) => Functor (g :. f) where
--  fmap fun (O gfa) = O $ fmap (fmap fun) $ gfa
  fmap fun (O gfa) = O $ (fun <$>) <$> gfa
~~~~

# Składanie idiomów

Składanie monad jest trudne (i nie zawsze możliwe). 

Składanie idiomów jest łatwe (no, prawie)

~~~~ {.haskell}
-- g :: * -> *,  f :: * -> * => g :. f :: * -> *
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  -- pure :: a -> (g :. f) a 
  --      ~~ a -> g (f a) 
  pure  = O . pure . pure
  
  -- (<*>) :: (g :. f) (a -> b) -> (g :. f) a -> (g:. f b)
  --       ~~ g(f(a ->b)) -> g(f(a)) -> g(f(b))
  O gs <*> O xs = -- O (| (<*>) gs xs |) 
                  O ( (<*>) <$> gs <*> xs)
~~~~

<!--
# Ćwiczenie

**Ćwiczenie:** zdefiniować

~~~~ {.haskell}
instance (Functor g, Functor f) => Functor (g :. f) where ...
~~~~

i sprawdzić, że złożenie funktorów aplikatywnych spełnia prawa dla funktora aplikatywnego.
-->

# Kategoryjnie: strong lax monoidal functor

Operacja `<*>` jest asymetryczna, ale jest równoważna definicja symetryczna:

~~~~ {.haskell}
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)
  
instance Applicative f => Monoidal f where
  unit = pure ()
  pair fa fb = (,) <$> fa <*> fb
  
instance Monoidal f => Applicative f where  
  pure x      = fmap (\() -> x) unit
             -- fmap (const x) unit 
    mf <*> mx = (\(f,x) -> f x) <$> pair mf mx
             -- uncurry ($) <$> pair mf mx
~~~~

Żeby uzyskać prawdziwą równoważność trzeba oczywiście mieć pewne prawa
dla Monoidal. Okazuje się, że jest to coś, co w teori kategorii nazywa
się *strong lax monoidal functor* ;-)

# Parsery

Zauważmy natomiast, że `Monoidal` jest naturalną strukturą dla parserów

~~~~ {.haskell}
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)

emptyP :: Parser ()
thenP :: Parser a -> Parser b -> Parser (a,b)
~~~~

tyle, że typy robią się skomplikowane, dlatego łatwiej używać `Applicative`

~~~~ {.haskell}
-- S   ->  ( S ) S | epsilon
parens = (\_ s _ s2 -> max (1+s) s2) <$> 
         char '(' <*> parens <*> char ')' <*> parens 
         <|> pure 0
~~~~

# Alternative

Potrzebujemy jeszcze tylko alternatywy:

~~~~ {.haskell}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
~~~~

spełniającej aksjomaty monoidu. Patrz też MonadPlus:

~~~~ {.haskell}
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

-- mzero >>= f  =  mzero
-- v >> mzero   =  mzero
~~~~

**Ćwiczenie:** napisz parser dla wyrażeń arytmetycznych, uzywając tylko idiomów (bez `do` i bez `>>=`)

# Koniec

~~~~ {.haskell}

~~~~
