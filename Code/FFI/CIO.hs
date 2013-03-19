{-# LANGUAGE ForeignFunctionInterface #-}
module CIO(ugetchar,uputchar) where
import Foreign.C -- get the C types
import System.IO.Unsafe 

foreign import ccall "stdio.h getchar" cgetchar :: Int -> Char
foreign import ccall "stdio.h putchar" cputchar  :: Char -> ()
foreign import ccall "eof.h eof_stdin" ceof :: Int -> Int

{-# NOINLINE ugetchar #-}
ugetchar :: () -> Char
ugetchar () = case ceof 0 of
         0 -> cgetchar 0
         _ -> '\0'

uputchar = cputchar
