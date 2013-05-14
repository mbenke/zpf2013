import Data.IORef

main = do
  px <- newIORef undefined
  writeIORef px 42
  readIORef px >>= print
