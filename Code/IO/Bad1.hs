module Bad1 where
import Impure

printStr :: String -> [()]
printStr = map iputchar

main :: IO ()
main = printStr "Hello\n" `seq` return () 