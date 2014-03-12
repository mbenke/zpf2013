module Dialogue2 where
import qualified System.IO as Sys       
import qualified System.Environment as Environment
import System.IO.Error(catchIOError)
import Debug.Trace

type Dialogue = [Response] -> [Request]

data Response 
  = Success
  | Chr Char
  | Chan Chan
  | Str String
  | StrList [String]
  | Failure IOError
  deriving(Show)  

data Request    
  = GetChar Chan
  | PutChar Chan Char
  | OpenFile Path Mode
  | CloseChan Chan
  | GetArgs
  deriving(Show)    
          
type Chan = Sys.Handle
stdin = Sys.stdin
stdout = Sys.stdout

type Mode = Sys.IOMode
readMode = Sys.ReadMode
writeMode = Sys.WriteMode

type Path = String

runDialogue :: Dialogue -> IO()
runDialogue d = case (d undefined) of
     [] -> return ()
     (q:qs) -> do 
       r <- (runRequest q )
       runDialogue $ \rs -> tail (d (r:rs))
runRequest :: Request -> IO Response
runRequest r = runR r `catchIOError` \e -> return (Failure e)

runR (PutChar h c) = Sys.hPutChar h c >> return Success
runR (GetChar h ) = do
  eof <- Sys.hIsEOF h
  if eof then return (Chr '\0') else fmap Chr (Sys.hGetChar h)
runR (OpenFile m p) = fmap Chan (Sys.openFile m p)
runR (GetArgs) = fmap StrList Environment.getArgs
runR (CloseChan h) = Sys.hClose h >> return Success

test :: Dialogue
test rs =  [OpenFile "/dev/null" readMode, GetChar h, PutChar stdout c] where
  (Chan h:rs') = rs  
  (Chr c:rs'') = rs'
  (Success:_) = rs''

  
