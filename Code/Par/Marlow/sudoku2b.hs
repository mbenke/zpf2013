import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (force (map solve as))
       b <- rpar (force (map solve bs))
       rseq a
       rseq b
       return ()
