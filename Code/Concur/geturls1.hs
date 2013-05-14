import GetURL
import Control.Concurrent

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar  
  forkIO $ do 
    writeln "START 1"
    r <- getURL "http://www.wikipedia.com/wiki/Shovel"
    putMVar m1 r
    
  forkIO $ do 
    writeln "START 2"
    r <- getURL "http://www.wikipedia.com/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  writeln "1 DONE"  
  r2 <- takeMVar m2
  writeln "2 DONE"
  
writeln = putStrLn
  