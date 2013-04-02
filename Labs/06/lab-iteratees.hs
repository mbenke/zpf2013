-- Iteratee

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


-- getchar

getchar :: Monad m => Iteratee el m (Maybe el)
getchar = undefined


-- count_i

count_i :: Monad m => Iteratee el m Int
count_i = undefined


-- run

run :: Monad m => Iteratee el m a -> m a
run = undefined


-- Enumerator

type Enumerator el m a =
  Iteratee el m a -> m (Iteratee el m a)


-- enum_list

enum_list :: Monad m => [el] -> Enumerator el m a
enum_list = undefined


-- kompozycja sekwencjalna

(>>>) :: Monad m => Enumerator el m a
  -> Enumerator el m a
  -> Enumerator el m a
(>>>) = undefined


-- enumerator pusty

eof :: Monad m => Enumerator el m a
eof = undefined


-- Enumeratee

type Enumeratee elo eli m a =
  Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)


-- pipe

infixr 1 .|
(.|) :: Monad m => (Iteratee el m a -> w)
  -> Iteratee el m (Iteratee el' m a)
  -> w
(.|) = undefined


-- en_filter

en_filter :: Monad m => (el -> Bool) -> Enumeratee el el m a
en_filter = undefined


-- take

take :: Monad m => Int -> Enumeratee el el m a
take = undefined


-- drop

drop :: Monad m => Int -> Enumeratee el el m a
drop = undefined


-- pair

en_pair :: Monad m => Iteratee el m a
  -> Iteratee el m b
  -> Iteratee el m (a,b)
en_pair = undefined
