import Expr

simpl :: Exp -> Exp
simpl (EAdd (EInt 0) x) = x