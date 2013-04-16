{-# LANGUAGE  QuasiQuotes #-}
import ExprQuote2
import Expr2

simpl :: Exp -> Exp
simpl [expr|0 + $x|] = x

main = print $ simpl [expr|0+2|]
