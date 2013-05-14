import Data.IORef
import Control.Concurrent

incRef :: IORef Int -> IO ()
incRef var = do { val <- readIORef var
                ; threadDelay 1000         
       	        ; writeIORef var (val+1) }

main = do
  px <- newIORef 0
  forkIO $ incRef px
  forkIO $ incRef px
  threadDelay 3000
  readIORef px >>= print