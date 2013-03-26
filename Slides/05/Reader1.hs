type E = Int  -- na przykład
type RM a = E -> a

rmap :: (a->b) -> RM a -> RM b
rmap = (.)

rpure :: a -> RM a
rpure = const

rbind :: RM a -> (a -> RM b) -> RM b
-- (E -> a) -> (a -> E -> b) -> E -> b
rbind m k e = k (m e) e

rjoin :: RM (RM e) -> RM e
-- (E -> E -> a) -> (E -> a) 
rjoin mm e = mm e e
