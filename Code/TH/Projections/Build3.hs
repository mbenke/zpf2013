{-# LANGUAGE TemplateHaskell #-}
module Build3 where
import Language.Haskell.TH

simpleFun :: Name -> [Pat] -> Exp -> Dec
simpleFun name pats rhs = FunD name [Clause pats (NormalB rhs) []]

build_ps = mapM build_p [1,2] where
    fname n = mkName $ "p2_" ++ show n
    argString k = "a" ++ show k
    argStrings = map argString [1,2]
    build_p n = do    
        argNames <- mapM newName argStrings
        let args = map VarP argNames
        return $ simpleFun (fname n) [TupP args] (VarE (argNames !! (n-1)))

