{-# LANGUAGE ForeignFunctionInterface #-}
module CIO where
import Foreign.C -- get the C types
import System.IO.Unsafe 

foreign import ccall "stdio.h getchar" cgetchar :: IO Char
foreign import ccall "stdio.h putchar" cputchar  :: Char -> IO ()

ugetchar :: () -> Char
ugetchar ()= unsafePerformIO cgetchar

uputchar ::  Char -> ()
uputchar c = unsafePerformIO $ cputchar c 