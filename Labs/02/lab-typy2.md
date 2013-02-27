# Typy jako język programowania

*    Funkcje na typach obliczane w czasie kompilacji

    ~~~~ {.haskell}
    data Zero
    data Succ n

    type One   = Succ Zero
    type Two   = Succ One
    type Three = Succ Two
    type Four  = Succ Three

    one   = undefined :: One
    two   = undefined :: Two
    three = undefined :: Three
    four  = undefined :: Four

    class Add a b c | a b -> c where
      add :: a -> b -> c
      add = undefined
    instance              Add  Zero    b  b
    instance Add a b c => Add (Succ a) b (Succ c)
    ~~~~ 

    ~~~~
    *Main> :t add three one
    add three one :: Succ (Succ (Succ (Succ Zero)))
    ~~~~

* Ćwiczenie: rozszerzyć o mnoćenie i silnię

# Typy jako język programowania (2)
Wektory przy użyciu klas:

~~~~ {.haskell}
data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> xs) = x
~~~~

**Ćwiczenie:** dopisać `vtail`, `vlast`

# Klasy 

Uzupełnij brakujące definicje:

~~~~  {.haskell}
class Fluffy f where
  furry :: (a -> b) -> f a -> f b
 
-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = error "todo"
 
-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry = error "todo"
 
-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = error "todo"
 
newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)
 
-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry = error "todo"
 
-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry = error "todo"
 
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' = error "todo"
 
-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = error "todo"
  unicorn = error "todo"
 
-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana = error "todo"
  unicorn = error "todo"
 
-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana = error "todo"
  unicorn = error "todo"
 
-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana = error "todo"
  unicorn = error "todo"
 
-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana = error "todo"
  unicorn = error "todo"
 
-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = error "todo"
 
-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = error "todo"
 
-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"
 
-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"
 
-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"
 
-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"
 
-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"
 
newtype State s a = State {
  state :: (s -> (s, a))
}
 
-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"
 
-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
~~~~

źródło:

http://blog.tmorris.net/20-intermediate-haskell-exercises/

# Klasy wieloparametrowe

Rozważmy klasę Iso z wykładu:

~~~~ {.haskell}
{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
class Iso a b where
  iso :: a -> b
  osi :: b -> a

instance Iso a a where
  iso = id
  osi = id

instance Iso ((a,b)->c) (a->b->c) where
  iso = curry
  osi = uncurry

instance (Iso a b) => Iso [a] [b] where
 iso = map iso
 osi = map osi
~~~~

* Uwaga: w ostatnim przyk³adzie `iso` ma inny typ po lewej, inny po prawej 

* Ćwiczenie: napisz jeszcze jakieś instancje klasy `Iso`


    ~~~~ {.haskell}
    instance (Functor f, Iso a b) => Iso (f a) (f b) where ...
    instance Iso (a->b->c) (b->a->c) where ...
    instance (Monad m, Iso a b) => Iso (m a) (m b) where ...
    ~~~~

# Klasy konstruktorowe

Na listach mamy 

~~~~  {.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
~~~~

możemy to uogólnić na inne konstruktory typów:

~~~~  {.haskell}
import Prelude hiding(foldr)
import qualified Prelude

class Foldable t where
   foldr :: (a -> b -> b) -> b -> t a -> b
~~~~

(tu uproszcozne, patrz Data.Foldable)

**Ćwiczenie:** napisz instancje

~~~~  {.haskell}
Foldable []	 
Foldable Maybe	 
Ix i => Foldable (Array i)
~~~~

(gdzie Array to tablice z Data.Array)

Można take zdefiniować inne metody Foldable:

~~~~  {.haskell}
-- | Map each element of the structure to a monoid,
-- and combine the results.
foldMap :: (Monoid m) => (a -> m) -> t a -> m

foldl :: (a -> b -> a) -> a -> t b -> a
~~~~

oraz funkcje

~~~~ {.haskell}
-- | The concatenation of all the elements of a container of lists.
concat :: Foldable t => t [a] -> [a]
concat = fold

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists.
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
and :: Foldable t => t Bool -> Bool

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
or :: Foldable t => t Bool -> Bool

-- | Determines whether any element of the structure satisfies the predicate.
any :: Foldable t => (a -> Bool) -> t a -> Bool

-- | Determines whether all elements of the structure satisfy the predicate.
all :: Foldable t => (a -> Bool) -> t a -> Bool

-- | The 'sum' function computes the sum of the numbers of a structure.
sum :: (Foldable t, Num a) => t a -> a

-- | The largest element of a non-empty structure.
maximum :: (Foldable t, Ord a) => t a -> a

-- | Does the element occur in the structure?
elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
~~~~
