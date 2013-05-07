import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Control.Seq as Seq
import Control.DeepSeq

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    evaluate $ force $ runEval $ parMap solve grids
    return ()

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)

