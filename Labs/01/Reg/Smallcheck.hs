{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Monad(liftM2)
import Control.Applicative((<$>),(<*>))

import Mon
import Reg
import RegExtra

equivRefl :: Reg AB -> Bool
equivRefl x = x === x

-- equivCompatible :: Monad m => Reg AB -> Reg AB -> Property m
equivCompatible :: Reg AB -> Reg AB -> Bool
equivCompatible c d = not ((Lit c) === (Lit d)) || c == d

leftUnit :: Reg AB -> Bool
leftUnit x = m1 <> x === x

rightUnit :: Reg AB -> Bool
rightUnit x =  x <> m1 === x

assoc :: Reg AB -> Reg AB -> Reg AB -> Bool
assoc x y z = (x<>y)<>z === x<>(y<>z)

nullableUnit :: Bool
nullableUnit = nullable m1

-- nullableOp :: Reg AB -> Reg AB -> Reg AB -> Property
nullableOp x y z= nullable x ==> nullable y ==>  nullable (x <> y)

iff :: Bool -> Bool -> Bool
iff a b = (a && b) || (not a && not b)

nullableSimpl, emptySimpl :: Reg AB -> Bool
nullableSimpl x = nullable x `iff` nullable (simpl x)
emptySimpl x = empty x `iff` empty (simpl x)

{-
recLeftNul :: Reg AB -> Property
recLeftNul y = forAllNullable $ \x ->  
               forAllMatching y $ \cs -> 
               accepts y cs ==> accepts (x:>y) cs

recRightNul :: Reg AB -> Property
recRightNul x = forAllNullable $ \y ->  
               forAllMatching x $ \cs -> 
               accepts x cs ==> accepts (x:>y) cs
-}
write = putStr
writeln = putStrLn
main = do 
     smallCheck 3 equivRefl
     smallCheck 1 equivCompatible
     smallCheck 3 leftUnit
     smallCheck 3 rightUnit
     writeln "assoc x y z = (x<>y)<>z == x<>(y<>z)"
     smallCheck 2 assoc
--     smallCheck 3 nullableSimpl


instance Monad m => Serial m AB where
  series = cons0 A \/ cons0 B


-- instance Serial IO AB
-- instance Serial m a => Serial m (Reg a)

instance (Monad m, Serial m c) => Serial m (Reg c) where
         series =  cons0 Eps 
                \/ cons0 Empty
                \/ cons1 Lit
                \/ cons1 Many
                \/ cons2 (:>)
                \/ cons2 (:|)

        