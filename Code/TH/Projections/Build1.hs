{-# LANGUAGE TemplateHaskell #-}
module Build1 where
import Language.Haskell.TH

build_p1 :: Q [Dec]
build_p1 = return
    [ FunD p1 
             [ Clause [TupP [VarP a,VarP b]] (NormalB (VarE a)) []
             ]
    ] where
       p1 = mkName "p1"
       a = mkName "a"
       b = mkName "b"
