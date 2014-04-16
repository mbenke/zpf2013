\begin{code}
{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
\end{code}

Get/Set
=======

\begin{code}
data YMD = YMD { ymdYear :: Year, ymdMonth :: Month,
                 ymdDay :: DayOfMonth } 
data DateTime = DT { dtDate :: YMD, dtTime :: Time } deriving Show

getDTMonth :: DateTime -> Month
getDTMonth = ymdMonth . dtDate

setDTMonth :: Month -> DateTime -> DateTime
setDTMonth m dt = dt { dtDate = (dtDate dt) { ymdMonth = m } } 
-- Lens DateTime Month ~ { get :: DateTime -> Month, set :: Month -> DateTime -> DateTime }
\end{code}

Kłopotliwe...

Pierwsze podejście
==================

\begin{code}
type Get s a = s -> a
type Set s a = a -> s -> s
data LensR s a = LensR { getR :: s -> a, setR :: a -> s -> s }
composeL ::  forall s a b.LensR s a -> LensR a b -> LensR s b
composeL lsa lab = LensR { getR = getR lab . getR lsa, setR = setsb } where
  setsb :: Set s b
  setsb newb s = seta (setb newb (geta s)) s
    -- let newa  = setb newb (geta s) in seta newa s
  geta :: Get s a
  geta = getR lsa
  seta = setR lsa
  setb :: Set a b
  setb = setR lab 
\end{code}

* Skomplikowane
* Nieefektywne

\begin{code}
modify :: LensR s a -> (a->a) -> s -> s
modify ln f s = setR ln (f (getR ln s)) s  
\end{code}

Modyfikacje
===========

< modify :: LensR s a -> (a->a) -> s -> s
< modify ln f s = setR ln (f (getR ln s)) s  

Moglibyśmy dodać modify do rekordu LensR, ale co jeśli potrzeba

< modMaybe :: LensR s a -> (a->Maybe a) -> s -> Maybe s
< modifyIO :: LensR s a -> (a->Maybe a) -> s -> Maybe s

Może wystarczyłoby `modifyM`, ale lepiej użyć ogólnego modify dla funktorów

\begin{code}
type Lens' s a = forall f. Functor f => 
                       (a -> f a) -> s -> f s
\end{code}

Lens' - set
===========


< Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

 co bedzie dla `f a=a`?

< Lens' s a@Identity = (a -> a) -> s -> s

czyli modify

\begin{code}
newtype Identity a = Identity { runIdentity :: a}
instance Functor Identity where
   fmap f = Identity . f . runIdentity

set :: forall s a.Lens' s a -> a -> s -> s
set lens newa s = runIdentity (lens setField s) where
  setField :: a -> Identity a
  setField olda = Identity newa
\end{code}

Krócej, bezpunktowo:

< set :: Lens' s a -> a ->s ->s
< set lens newa = runIdentity . lens (Identity . const newa)

Appendix
=======
\begin{code}
type Year = Int
type Month = Int
type DayOfMonth = Int
type Time = Int
\end{code}
