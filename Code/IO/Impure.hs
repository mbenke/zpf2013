{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-} 
module Impure where
import System.IO.Unsafe
import System.IO

{-# NOINLINE igetchar #-}
igetchar :: () -> Char
igetchar () = unsafePerformIO $ do
  b <- isEOF
  if b then return '\0'  else getChar


{-# NOINLINE iputchar #-}
iputchar :: Char -> ()
iputchar = unsafePerformIO . putChar