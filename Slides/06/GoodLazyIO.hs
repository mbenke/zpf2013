import System.IO

main = withFile "Makefile" ReadMode $ \handle -> do
  fileData <- hGetContents handle
  putStr fileData
