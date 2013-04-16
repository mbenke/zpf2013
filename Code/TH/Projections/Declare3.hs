{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

import Build3

$(build_ps)

main = mapM_ print 
  [ p2_1 (1,2)
  , p2_2 (1,2)
  ]