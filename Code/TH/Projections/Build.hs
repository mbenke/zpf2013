{-# LANGUAGE TemplateHaskell #-}
module Projections.Build where
import Language.Haskell.TH
import Language.Haskell.TH.Ppr(ppr)
import Language.Haskell.TH.PprLib(Doc)

-- Language.Haskell.TH> runQ [d| p2_1 (a,b) = a |]
-- [FunD p2_1_3 [Clause [TupP [VarP a_4,VarP b_5]] (NormalB (VarE a_4)) []]]

defs_pa21 :: Q [Dec]
defs_pa21 = return  [FunD p21 [Clause [TupP [VarP a,VarP b]] (NormalB (VarE a)) []]] where
          [p21, a,b] = map mkName  ["pa2_1", "a", "b"]

simpleFun :: Name -> [Pat] -> Exp -> Dec
simpleFun name pats rhs = FunD name [Clause pats (NormalB rhs) []]

defs_pb21 :: Q [Dec]
defs_pb21 = do
    let p21 = mkName "pb2_1"
    a <- newName "a"
    b <- newName "b"
    return [simpleFun p21 [TupP [VarP a,VarP b]] (VarE a)]

defs_pc2 :: Q [Dec]
defs_pc2 = mapM def_pc2 [1,2] where
    fname n = mkName $ "pc2_" ++ show n
    argString k = "a" ++ show k
    argStrings = map argString [1,2]
    def_pc2 n = do    
        argNames <- mapM newName argStrings
        let args = map VarP argNames
        return $ simpleFun (fname n) [TupP args] (VarE (argNames !! (n-1)))

ppr_pc2 :: Q String
ppr_pc2 = fmap (show . ppr) defs_pc2

print_pc2 :: IO ()
print_pc2 = runQ ppr_pc2 >>= putStrLn
