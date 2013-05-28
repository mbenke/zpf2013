
-- Swierstra & Duponcheel LL(1) parsers

-- static kinfo: nullable, First
data StaticParser s = SP { spNullable :: Bool, spFirst :: [s] } 
newtype DynamicParser s a = DP([s] -> (a,[s]))
data Parser s a = P (StaticParser s)(DynamicParser s a)

symbol :: s -> Parser s s
symbol s = P (SP False [s]) (DP (\(x:xs) -> (s,xs)))

(<|>) :: Eq s => Parser s a -> Parser s a -> Parser s a
(P (SP nul1 first1) (DP p1)) <|> (P (SP nul2 first2) (DP p2)) =
  P (SP (nul1 || nul2) (first1++first2)) (DP p) where
    p xs = case xs of
      []     -> if nul1 then p1 [] else p2 []
      (y:ys) -> if y `elem` first1 then p1 xs else   
                if y `elem` first2 then p2 xs else
                if nul1 then p1 xs else p2 xs
-- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b ??
-- 

<*> :: (Parser s (a->b)) -> Parser s a -> Parser s b
(P (SP nul1 first1) (DP pf)) <|> (P (SP nul2 first2) (DP pa)) = 
  P (SP (nul1&&nul2) first) (DP p) where 
    first = if nul1 then first1 ++ first2 else first1
    p = undefined