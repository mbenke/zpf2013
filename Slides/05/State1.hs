{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
import Data.Monoid

-- Dla dowolnego c operacja \ a -> (a,c) jest funktorem:
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- podobnie \b -> (c,b)
second :: (b->d) -> (c,b) -> (c,d)  
second f (c,b) = (c, f b)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
f &&& g = \a -> (f a, g a)
-- first f = f &&& id
-- second f = id &&& f
p1 :: Monoid c => a -> (a,c)
p1 a = (a,mempty)

type S = Int  -- przykładowo
type SM a = S -> (a,S)

smap :: (a->b) -> (SM a -> SM b)
smap f t = first f . t -- \s -> ffirst f (t s)
-- Nie można napisać instance Functor SM ...

spure :: a -> SM a
spure a s = (a, s)
-- spure = (,)

sbind :: SM a -> (a -> SM b) -> SM b
sbind f k = \s -> let (a,s') = f s in k a s'

-- sbind' :: a -> SM b -> SM a -> SM b

-- join mma = bind m id

sjoin :: SM (SM a) -> SM a
-- sjoin :: (S -> (S -> (a,S),S)) -> S -> (a,S)
sjoin mma = \s -> let (ma,s') = mma s in ma s'


-- uncurry ($) :: (b -> c, b) -> c
sjoin' :: SM (SM a) -> SM a
-- sjoin' mma = \s -> uncurry ($) (mma s)
-- sjoin' mma = uncurry ($) . mma
sjoin' = (uncurry ($) .) 