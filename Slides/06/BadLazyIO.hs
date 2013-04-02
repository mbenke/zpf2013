import System.IO

main = do
	fileData <- withFile "Makefile" ReadMode hGetContents
	putStr fileData
