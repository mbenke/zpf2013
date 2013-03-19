{-# LANGUAGE ForeignFunctionInterface #-}
module FfiExample where
import Foreign.C -- get the C types
 
-- pure function
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
mysin :: Double -> Double
mysin d = realToFrac (c_sin (realToFrac d))