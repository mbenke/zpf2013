add1 :: Monad m => m Int -> m Int -> m Int 
add1 mx my = mx >>= \x-> my >>= \y -> return $ x+y
-- liftM2 (+)


(?) = undefined

addInt :: Int -> Int -> Int
addInt = (+)


-- infixr 5 <+>
infixr 3 ***
infixr 3 &&&
-- infixr 2 +++
-- infixr 2 |||
-- infixr 1 ^>>, >>^
-- infixr 1 ^<<, <<^
infixr 1 >>>, <<<

class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)
    second :: a b c -> a (d, b) (d, c)
    second g = arr swap >>> first g >>> arr swap 
      where swap (x,y) = (y,x)
    (<<<) :: a c d -> a b c -> a b d
    (<<<) = flip (>>>)
    
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
f *** g = first f >>> second g

(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
f &&& g = arr dup >>> (f *** g)
  where dup x = (x,x)
                
add2 :: Arrow a => a b Int -> a b Int -> a b Int
add2 f g = (f &&& g) >>> arr (\(u,v) -> u + v)
                           -- uncurry (+)