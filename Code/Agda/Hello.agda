module Hello where
-- open import IO.Primitive
open import IO
open import Data.String


main = run (putStrLn "Hello") -- (toCostring "Ello")