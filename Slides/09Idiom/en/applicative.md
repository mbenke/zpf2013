# Reminder - Functors

A functor is an operation `T :: * -> *` o types
with an operation `fmap` on functions

~~~~ {.haskell}
fmap :: (a -> b) -> (T a -> T b) 
~~~~

preserving function composition, e.g.

~~~~
fmap id = id
fmap (f . g) = fmap f . fmap g
~~~~

# Motivation - sequences

Let's recall a parser for digits:

~~~~ {.haskell}
pNum :: Parser Int
pNum = fmap digitToInt digit
~~~~

which is a shorthand for

~~~~ {.haskell}
pNum = do
     d <- digit
     return $ digitToInt d
~~~~

similarly we would like to simplify

~~~ {.haskell}
do x1 <- m1
   x2 <- m2
   return $ f x1 x2
~~~

#  Motivation - sequences

Recall the monadic `sequence` function:

~~~~ {.haskell}
sequence1 :: Monad m => [m a] -> m [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)
~~~~

it joins a list of actions collecting their results

We can simplify it using ``monadic'' application, `ap`:

~~~~ {.haskell}
sequence2 (c:cs) = return (:) `ap` c `ap` sequence cs

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do { f <- mf; x <- mx; return $ f x }
~~~~

# Motivation - transposing matrices

A matrix as a list of rows (of equal length)

~~~
> transpose [[1,2],[3,4],[5,6]]
[[1,3,5],[2,4,6]]

Idea:

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

# Connection with `zip`

~~~~ {.haskell}
consAll :: [a] -> [[a]] -> [[a]]
consAll (y:ys) (zs:zss) = (y:zs):consAll ys zss
consAll _      _        = []

zip :: [a] -> [b] -> [(a, b)]
zip  (y:ys) (z:zs) = (y,z):zip ys zs
zip  _      _      = []

-- Generalised zip
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (y:ys) (z:zs) = f y z:zipWith f ys zs
zipWith _  _      _      = []

zip = zipWith (,)
consAll = zipWith (:)
~~~~

NB `zip [1,2,3] (repeat 0) = [(1,0),(2,0),(3,0)]`

# transpose, again

Note

~~~~ {.haskell}
> zipWith (:) [1,2,3] (repeat [])
[[1],[2],[3]]
~~~~

hence one can write

~~~~ {.haskell}
transpose2 :: [[a]] -> [[a]]
transpose2 (xs:xss) = zipWith (:) xs (transpose2 xss)
transpose2 [] = repeat []
~~~~

problematic empty matrix case, but

~~~
> transpose $ transpose []
[]
> transpose [[]]
[]
~~~

# Motivation - `zipWith`

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

NB zap ≠ ap

~~~
*Main> ap [(+1)] [1,2]
[2,3]
*Main> zap [(+1)] [1,2]
[2]
~~~

# Motivation - interpreter

~~~~ {.haskell}
data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

eval1 :: Exp v -> Env v -> Int
eval1 (Var x) env = fetch x env
eval1 (Val i) env = i
eval1 (Add p q) env = eval1 p env + eval1 q env

-- Drop the boring env
eval2 (Var x) = fetch x
eval2 (Val i) = const i
eval2 (Add p q) = const (+) `apply` eval2 p 
                            `apply` eval2 q

apply :: (env -> a -> b) -> (env -> a) -> (env -> b)
apply ef es env = (ef env) (es env)
~~~~

# `Applicative` class

~~~~ {.haskell}
-- Control.Applicative
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
~~~~

Correspondence to monadic `ap`:

~~~ {.haskell}
ap :: Monad m => m (a -> b) -> m a -> m b
~~~

Example:

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

# Laws

~~~~
fmap g x = pure g <*> x
pure id <*> u = u (konsekwencja powyższego i praw fmap)
pure (.) <*> u <*> v <*> w =  u <*> v <*> w
pure f <*> pure x = pure (f x)
u <*> pure x = pure (\f -> f x) <*> u
~~~~

In the applicative style, `fmap` is written as infix `<$>`:

~~~~ {.haskell}
f <$> u = pure f <*> u
~~~~

**Exercise:** verify that the above laws hold for the given instance for `Maybe`

~~~~ {.haskell}
instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _ = Nothing
~~~~

# Examples, idiomatically

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

This is the same function:

~~~~ {.haskell}
dist :: Applicative f => [f a] -> f [a]
dist []     = pure []
dist (x:xs) = (:) <$> x <*> dist xs
~~~~

distributivity of `f` over lists

# Functional iterator

`dist` can be used with `map`, e.g.

~~~~ {.haskell}
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap m as = dist (map m as)
~~~~

...but it walks the list twice - once is enough:

~~~~ {.haskell}
traverseL :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseL f []     = pure []
traverseL f (x:xs) = (:) <$> f x <*> traverseL f xs
~~~~

We can generalise to arbitrary iterable structures:

~~~~ {.haskell}
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist = traverse id
  traverse f = dist . fmap f
~~~~

NB this definition comes from thge oreiginal McBride&Paterson paper - currently `Data.Traversable` uses a slightly different def

**Exercise:** write a `Traversable` instance for trees


# Idiom brackets

Conor McBride proposed a notation for idioms:

~~~~ 
 (|f a1 .. an|) = pure f <*> a1 <*> .. <*> an
sequence4 (c:cs) = (| (:) c (sequence4 cs) |)
eval4  (Add p q) = (| (+) (eval3 p) (eval3 q) |)
~~~~

it was not adopted in tyhe Haskell standard, though is available in SHE 
(Strathclyde Haskell Enhancement):

https://personal.cis.strath.ac.uk/~conor/pub/she/

# Ixdiom brackets

According to Conor, we can *fake it* using classes:

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

A slightly less hacky solution is to use Template Haskell:

~~~~
-- package applicative-quoters
 [i| subtract [1,2,3] [10,20,30] |]
 -> pure subtract <*> [1,2,3] <*> [10,20,30]
 -> [9,19,29,8,18,28,7,17,27]
~~~~

# Idioms vs monads

Every monad is applicative e.g.

~~~~ {.haskell}
instance Applicative Maybe where
  pure = return
  (<*>) = ap

ap mf mx = mf >>= \f -> m x >>= \x -> return (f x)

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }

instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . return
    WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
~~~~

**Exercise:** verify that applicative laws follow from monad laws

# Idioms vs monads

Every monad is applicative but in general the converse is not true, e.g. our instance for lists

~~~~ {.haskell}
instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
~~~~

can one define `>>=` so that `ap` corresponds to `<*>` ?


# Idioms vs monads

Manadic structure of lists corresponds to another Applicative instance, where every function is applied to every argument:

~~~~ {.haskell}
instance Applicative [] where
  pure = (:[])
  fs <*> xs = concat $ for fs (for xs)
  
for = flip map
~~~~

**Exercise:** check type correctness of the above definition

**Ćwiczenie:** write two `Applicative` instances for `State`

# Idioms vs monads

In general monadic sequencing is stronger than the idiomatic one:

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

~~~~ {.haskell}
instance Monoid [] where
  mempty = []
  mappend = (++)
~~~~

# Endo

~~~~ {.haskell}
-- | The monoid of endomorphisms under composition.
newtype Endo a = Endo { appEndo :: a -> a }
               deriving (Generic)

instance Monoid (Endo a) where
        mempty = Endo id
        Endo f `mappend` Endo g = Endo (f . g)
~~~~

# Monoid, applicatively

~~~~ {.haskell}
-- phantom type: does not use its argument
newtype Accy o a = Acc{acc::o}

instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  Acc o1 <*> Acc o2 = Acc (o1 <> o2)
~~~~

not monadic - how to define

~~~~ {.haskell}
(>>=) :: Accy o a -> (a->Accy o b) -> Accy o b
~~~~

# Accumulating errors

~~~~ {.haskell}
data Except err a = Ok a | Failed err

instance Monoid err => Applicative (Except err) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Failed err = Failed err
  Failed err <*> Ok _ = Failed err
  Failed e1 <*> Failed e2 = Failed (e1 <> e2)
~~~~

difficult to find the monadic analogue (ther is `Writer`, but it's something different)


# Example

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

Using monads we can report the first error, but how to catch them all?

# `condMap` monadically

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

# `condMap` idiomatically

We can write `condMap` idiomatically using the ``monadic'' Applicative for `Either` and the laws

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

# condMap idiomatically

Now just a small change

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

# Composing functors

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

# Composing idioms

Composing monads is difficult (and not always possible).

Composing idioms is easy (well, almost)

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

# Categorically: strong lax monoidal functor

The `<*>` operation is asymmetric, but there is an equivalent symmetric definition:

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

for a real ewquivalence we of course need certain laws for `Monoidal`.
It turns out the needed laws specify something known in category theory as
*strong lax monoidal functor* ;-)

# Parsery

Note that `Monoidal` seems a good fit for parsers:

~~~~ {.haskell}
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)

emptyP :: Parser ()
thenP :: Parser a -> Parser b -> Parser (a,b)
~~~~

however for bigger parsers, the types get complicated, so it's easier to use  `Applicative`:

~~~~ {.haskell}
-- S   ->  ( S ) S | epsilon
parens = (\_ s _ s2 -> max (1+s) s2) <$> 
         char '(' <*> parens <*> char ')' <*> parens 
         <|> pure 0
~~~~

# Alternative

Now we just need an alternative

~~~~ {.haskell}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
~~~~

with monoid laws. See also `MonadPlus`:

~~~~ {.haskell}
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

-- mzero >>= f  =  mzero
-- v >> mzero   =  mzero
~~~~

**Exercise:** write a parser for arithmetic expressions using just idioms (no `do` or `>>=`)

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

**Exercise:**

* write a few `Foldable` instances
* express `foldr` using `foldMap` and vice versa
* write a function

~~~~ {.haskell}
toList :: Foldable t => t a -> [a]
~~~~

# Data.Traversable

``traverse is the same as fmap, except it also allow you to run effects while you're rebuilding the data structure'' - Sjoerd Visscher

~~~~ {.haskell}
class (Functor t, Foldable t) => Traversable t where
    -- | Map each element of a structure to an action, evaluate
    -- these actions from left to right, and collect the results.
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right,
    -- and collect the results.
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

    -- | Map each element of a structure to a monadic action, evaluate
    -- these actions from left to right, and collect the results.
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM f = unwrapMonad . traverse (WrapMonad . f)

    -- | Evaluate each monadic action in the structure from left to right,
    -- and collect the results.
    sequence :: Monad m => t (m a) -> m (t a)
    sequence = mapM id
    {-# MINIMAL traverse | sequenceA #-}
~~~~

# THE END

~~~~ {.haskell}

~~~~
