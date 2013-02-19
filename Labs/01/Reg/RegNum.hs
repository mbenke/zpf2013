{-# LANGUAGE FlexibleInstances #-}
module RegNum where
import Reg
instance Num (Reg Integer) where
  fromInteger = Lit
  (+) = (:|)
  (*) = (:>)
  abs = undefined
  signum = undefined
  
r1 :: Reg Integer
r1 = Many (0*1 + 1*0) * 1 
