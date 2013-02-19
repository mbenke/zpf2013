module Reg where

data Reg c
  = Lit c
  | Reg c :> Reg c
  | Reg c :| Reg c
  | Many (Reg c)
  | Eps
  | Empty
  deriving (Eq,Show)

size :: Reg c -> Int
size (x :> y) = size x + size y + 1
size (x :| y) = size x + size y + 1
size (Many x) = 1 + size x
size _ = 1