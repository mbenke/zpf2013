import GetURL
import Control.Concurrent

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar  
  forkIO $ do 
    r <- getURL "http://www.wikipedia.com/wiki/Shovel"
    putMVar m1 r
    
  forkIO $ do 
    r <- getURL "http://www.wikipedia.com/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  print "1 DONE"  
  r2 <- takeMVar m2
  print "2 DONE"