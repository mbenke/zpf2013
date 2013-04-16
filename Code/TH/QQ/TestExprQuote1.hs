{-# LANGUAGE  QuasiQuotes #-}
import ExprQuote1
import Expr

t1 :: Exp
t1 = [expr|1+2*3|]

f1 :: Exp -> String
f1 [expr| 1+2*3 |] = "Bingo!"
f1 _ = "Sorry, no bonus" 