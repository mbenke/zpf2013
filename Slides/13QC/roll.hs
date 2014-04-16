import System.Random
  ( StdGen       -- :: *
  , newStdGen    -- :: IO StdGen
  , randomR      -- :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  , split        -- :: RandomGen g => g -> (g, g)
                 -- rozdziela argument na dwa niezależne generatory
  -- instance RandomGen StdGen
  -- instance Random Int  
  )
  
roll :: StdGen -> Int
roll rnd = fst $ randomR (1,6) rnd
main = do 
  rnd <- newStdGen 
  let (r1,r2) = split rnd
  print (roll r1)
  print (roll r2)
  print (roll r1)
  print (roll r2)