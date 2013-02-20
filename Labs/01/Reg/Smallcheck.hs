{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Monad(liftM2)
import Control.Applicative((<$>),(<*>))

import Mon
import Reg
import RegExtra

reflAB :: AB -> Bool
reflAB x = x == x

reflReg :: Reg AB -> Bool
reflReg x = x === x

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

recLeftNul :: (Monad m) => Reg AB -> Reg AB -> [AB] -> Property m
recLeftNul x y cs = nullable x ==> accepts y cs ==> accepts (x:>y) cs

recRightNul :: (Monad m) => Reg AB -> Reg AB -> [AB] -> Property m
recRightNul x y cs = nullable y ==> accepts x cs ==> accepts (x:>y) cs
               
write = putStr
writeln = putStrLn
main = do
     writeln "reflReg"
     smallCheck 3 reflReg
     writeln "left unit"
     smallCheck 3 leftUnit
     writeln "right unit"
     smallCheck 3 rightUnit
     writeln "assoc x y z = (x<>y)<>z == x<>(y<>z)"
     smallCheck 2 assoc
     writeln "nullableSimpl"
     smallCheck 3 nullableSimpl
     writeln "emptySimpl"
     smallCheck 3 emptySimpl
     writeln "recLeftNul"
     smallCheck 2 recLeftNul
     writeln "recRightNul"
     smallCheck 2 recRightNul     
     
instance Monad m => Serial m AB where
  series = cons0 A \/ cons0 B


instance (Monad m, Serial m c) => Serial m (Reg c) where
         series =  cons0 Eps 
                \/ cons0 Empty
                \/ cons1 Lit
                \/ cons1 Many
                \/ cons2 (:>)
                \/ cons2 (:|)

        
