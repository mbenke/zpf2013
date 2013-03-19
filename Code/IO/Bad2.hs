module Bad2 where
import Impure

printStr :: String -> ()
printStr [] = ()
printStr (c:cs) = let
 () = iputchar c 
 () = printStr cs
 in ()

main :: IO()
main = printStr "Hello\n" `seq` return () 