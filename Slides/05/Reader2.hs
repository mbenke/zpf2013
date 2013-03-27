-- type E = Int  -- na przykład
type RM a = E -> a

rmap :: (a->b) -> RM a -> RM b
rmap = (.)

rpure :: a -> RM a
rpure = const

infixl 1 `rbind`
rbind :: RM a -> (a -> RM b) -> RM b
-- (E -> a) -> (a -> E -> b) -> E -> b
rbind m k e = k (m e) e

rjoin :: RM (RM e) -> RM e
-- (E -> E -> a) -> (E -> a) 
rjoin mm e = mm e e

ask :: RM E -- E -> E 
ask = id

asks :: (E -> a) -> RM a
-- (E -> a) -> E -> a 
asks = id

local :: (E -> E) -> RM a -> RM a
-- (E -> E) -> (E -> a) -> E -> a
-- local t m e = m (t e)
local = flip (.)

wheree :: RM a -> (E -> E) -> RM a
wheree = (.) -- m `wheree` t

type Name = String
data Exp = EC Int 
         | EV Name
         | Exp :+ Exp  
         | ELet Name Exp Exp

type E = Name -> Int

empty :: E
empty = const undefined 

ext :: Name -> Int -> E -> E
ext n1 v e n2 | n1 == n2  = v 
                 | otherwise = e n2

eval :: Exp -> RM Int
eval (EC i) = return i
eval (EV n) = asks ($n)
eval (e1 :+ e2) = liftR (+) (eval e1) (eval e2)
eval (ELet n e1 e2) = eval e1 `rbind` wheree (eval e2) . ext n 

liftR :: (a -> b -> c) -> RM a -> RM b -> RM c
-- (a -> b -> c) -> (E -> a) -> (E -> b) -> (E -> c)
liftR f ra rb e = f (ra e) (rb e)