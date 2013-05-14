import Control.Monad.Par

network :: IVar In -> Par Out
network inp = do
 [vf,vg,vh] <- sequence [new,new,new]
 
 fork $ do x <- get inp
           put vf (f x)
  
 fork $ do x <- get vf
           put vg (g x)
 
 fork $ do x <- get vf
           put vh (h x)
 
 x <- get vg
 y <- get vh
 return (j x y)
 
runNetwork x = runPar $ do 
  inpVar <- new
  put inpVar x
  network inpVar
  
main = print $ runNetwork 2

type In = Int
type Out = (Int,Int)
f,g,h :: Int -> Int
f x = x+1
g x = x+x
h x = x*x
j = (,) 