import Data.List(union) 

-- Swierstra & Duponcheel LL(1) parsers, arrow version

-- static kinfo: nullable, First
data StaticParser s = SP { spNullable :: Bool, spFirst :: [s] } 
newtype DynamicParser s a b = DP( (a,[s]) -> (b,[s]) )
data Parser s a b = P (StaticParser s)(DynamicParser s a b)

symbol :: s -> Parser s a s
symbol s = P (SP False [s]) (DP (\(a,x:xs) -> (s,xs)))

class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)

instance Arrow (DynamicParser s) where
  arr f =  (DP (\(a,xs) -> (f a, xs)))
  (>>>) = undefined
  first = undefined
  
instance Eq s => Arrow (Parser s) where
   arr f = P (SP True []) (arr f)
   
   (P (SP nul1 first1) dp1) >>> (P (SP nul2 first2) dp2) =
     P (SP (nul1 && nul2) (union first1 first2)) (dp1 >>> dp2) 
     
   first (P sp dp) = P sp (first dp)
   
class Arrow a => ArrowZero a where
    zeroArrow :: a b c
   
instance Eq s => ArrowZero (Parser s) where
  zeroArrow = P (SP False []) zeroArrow
  
instance ArrowZero (DynamicParser s) where
  zeroArrow = (DP (\(a,xs) -> error "zero"))
  
class ArrowZero a => ArrowPlus a where
    -- | An associative operation with identity 'zeroArrow'.
    (<+>) :: a b c -> a b c -> a b c  
    
instance Eq s => ArrowPlus (Parser s) where
  (<+>) = undefined
  
parseInt :: Parser Char a Int
parseInt = arr (const 0) >>> parseInt'

parseInt' :: Parser Char Int Int
parseInt' = undefined