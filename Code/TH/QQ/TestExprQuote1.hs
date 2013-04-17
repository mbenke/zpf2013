{-# LANGUAGE  QuasiQuotes #-}
import ExprQuote1
import Expr

-- show
testExp :: Exp
testExp = [expr|1+2*3|]

f1 :: Exp -> String
f1 [expr| 1 + 2*3 |] = "Bingo!"
f1 _ = "Sorry, no bonus" 

main = putStrLn $ f1 testExp
-- /show