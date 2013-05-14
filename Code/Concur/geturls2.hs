import GetURL

import Control.Monad
import Control.Concurrent
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyMVar
   forkIO (action >>= putMVar var)
   return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

main = do
  m1 <- async $ getURL "http://www.wikipedia.com/wiki/Shovel"
  m2 <- async $ getURL "http://www.wikipedia.com/wiki/Spade"
  wait m1
  writeln "1 DONE"  
  wait m2
  writeln "2 DONE"
  
writeln = putStrLn