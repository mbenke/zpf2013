{-# LANGUAGE  QuasiQuotes #-}
import ExprQuote2
import Expr2

t1 :: Exp
t1 = [expr|1+2*3|]

f1 :: Exp -> String
f1 [expr| $a + $b |] = "Bingo!"
f1 _ = "Sorry, no bonus" 

eval [expr| $a + $b|] = eval a + eval b
eval [expr| $a * $b|] = eval a * eval b
eval (EInt n) = n

test = eval [expr| 2+2 |]