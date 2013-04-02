import System.IO
import System.IO.Error(isEOFError)
import Control.Exception
import Data.Char(isSpace)

countSpaces :: Handle -> IO Int
countSpaces handle = loop 0
  where
  loop n = try (hGetChar handle) >>= check n
  check n (Right c) = loop (if isSpace c then n + 1 else n)
  check n (Left e) | Just ioe <- fromException e,
                     isEOFError ioe = return n
  check _ (Left e) = throw e
        
runCountSpaces fileName =
  bracket (openFile fileName ReadMode) hClose $ \handle ->
    countSpaces handle >>= print

main = runCountSpaces "Makefile"
