{-# LANGUAGE TemplateHaskell #-}
module Projections.Declare where
import Language.Haskell.TH

import Projections.Build

$(defs_pa21)
$(defs_pb21)

$(defs_pc2)