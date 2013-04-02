# Iteratee

~~~~ {.haskell}
data Chunk a
  = Chunk [a]
  | EOF
  deriving (Show, Eq)

data Step a m b
  = Continue (Chunk a -> Iteratee a m b)
  | Yield b (Chunk a)
  | Error

newtype Iteratee a m b = Iteratee {
  runIteratee :: m (Step a m b)
}
~~~~

## getchar

~~~~ {.haskell}
getchar :: Monad m => Iteratee el m (Maybe el)
getchar = undefined
~~~~

## count_i
~~~~ {.haskell}
count_i :: Monad m => Iteratee el m Int
count_i = undefined
~~~~

## run
~~~~ {.haskell}
run :: Monad m => Iteratee el m a -> m a
run = undefined
~~~~

# Enumerator
~~~~ {.haskell}
type Enumerator el m a =
  Iteratee el m a -> m (Iteratee el m a)
~~~~

## Listy
~~~~ {.haskell}
enum_list :: Monad m => [el] -> Enumerator el m a
enum_list = undefined
~~~~

## Kompozycja sekwencyjna
~~~~ {.haskell}
(>>>) :: Monad m => Enumerator el m a
  -> Enumerator el m a
  -> Enumerator el m a
>>> = undefined
~~~~

## Enumerator pusty
~~~~ {.haskell}
eof :: Monad m => Enumerator el m a
eof = undefined
~~~~

# Enumeratee
~~~~ {.haskell}
type Enumeratee elo eli m a =
  Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)
~~~~

## Pipe
~~~~ {.haskell}
infixr 1 .|
(.|) :: Monad m => (Iteratee el m a -> w)
  -> Iteratee el m (Iteratee el' m a)
  -> w
.| = undefined
~~~~

## Filter
~~~~ {.haskell}
en_filter :: Monad m => (el -> Bool) -> Enumeratee el el m a
en_filter = undefined
~~~~

## Take
~~~~ {.haskell}
take :: Monad m => Int -> Enumeratee el el m a
take = undefined
~~~~

## Drop
~~~~ {.haskell}
drop :: Monad m => Int -> Enumeratee el el m a
drop = undefined
~~~~

## Pair
~~~~ {.haskell}
en_pair :: Monad m => Iteratee el m a
  -> Iteratee el m b
  -> Iteratee el m (a,b)
en_pair = undefined
~~~~
