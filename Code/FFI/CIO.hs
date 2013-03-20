{-# LANGUAGE ForeignFunctionInterface #-}
module CIO(ugetchar,uputchar) where

foreign import ccall "stdio.h getchar" cgetchar :: Int -> Char
foreign import ccall "stdio.h putchar" cputchar  :: Char -> ()
foreign import ccall "eof.h eof_stdin" ceof :: Int -> Int
foreign import ccall "eof.h flush_stdout" cflush :: Int -> ()

{-# NOINLINE ugetchar #-}
ugetchar :: () -> Char
ugetchar () = case ceof 0 of
         0 -> cgetchar 0
         _ -> '\0'

{-# NOINLINE uputchar #-}
uputchar :: Char -> ()
uputchar c = case cputchar c of
         () -> cflush 0