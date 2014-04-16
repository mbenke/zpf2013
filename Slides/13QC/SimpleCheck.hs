module SimpleCheck where
import System.Random
  ( StdGen       -- :: *
  , newStdGen    -- :: IO StdGen
  , Random(..)   -- class
  , randomR      -- :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  , split        -- :: RandomGen g => g -> (g, g)
                 -- rozdziela argument na dwa niezależne generatory
    
  -- instance RandomGen StdGen
  )
import Data.List( group, sort, intersperse )
import Control.Monad( liftM2, liftM3, liftM4 )

-- infixr 0 ==>
-- infix  1 `classify`

newtype Gen a
  = Gen (Int -> StdGen -> a)

instance Monad Gen where
  return a = Gen $ \n r -> a
  Gen m >>= k = Gen $ \n r0 ->
    let (r1,r2) = split r0
        Gen m'  = k (m n r1)
     in m' n r2
        
instance Functor Gen where
  fmap f m = m >>= return . f        


rand :: Gen StdGen
rand = Gen (\n r -> r)

chooseInt1 :: (Int,Int) -> Gen Int
chooseInt1 bounds = Gen $ \n r  -> fst (randomR bounds r)
                     
chooseInt :: (Int,Int) -> Gen Int
chooseInt bounds = (fst . randomR bounds) `fmap` rand

choose ::  Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]

genOne :: Gen a -> IO a
genOne (Gen m) =
  do 
    rnd0 <- newStdGen
    return $ m 7 rnd0
  
-- * Arbitrary
class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b 

-- transformator generatorów
variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0

{-
sample' :: Gen a -> IO [a]
sample' (Gen m) =
  do rnd0 <- newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     return [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..20] ]
-}







data TestState = MkState { 
  randomSeed :: StdGen
}  


-- | The result of a single test.
data Result  = MkResult
  { ok          :: Maybe Bool     -- ^ result of the test case; Nothing = discard
  }  

{-
class Testable prop where
  property :: prop -> Property

simpleCheck :: Testable prop => prop -> IO Result
simpleCheck  p = do
  rnd <- newStdGen
  test MkState { 
    randomSeed = rnd
    } (property p)
    
test :: TestState -> Property -> IO Result
test = undefined
    
  -}