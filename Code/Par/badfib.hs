import Control.Parallel

parFib :: Int -> Int
parFib n | n < 2 = n
parFib n = p `par` q `pseq` (p + q)
    where
      p = parFib $ n - 1
      q = parFib $ n - 2

main :: IO ()
main = print $ parFib 40
