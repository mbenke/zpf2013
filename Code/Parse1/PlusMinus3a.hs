import MyParsec3a
import Data.Char(digitToInt)
import Criterion.Main

gen 0 = "1"
gen n = ('1':'+':'1':'-':gen (n-1))

pNum :: Parser Int
pNum = fmap digitToInt digit

pExp = pNum `chainl1` addop
addop   =   do{ char '+'; return (+) }
          <|> do{ char '-'; return (-) }

test n =  parse pExp "gen" (gen n)

main = defaultMain 
       [ {- bench "gen 10000" $ whnf test 10000
       , -} bench "gen 1e5" $ whnf test 100000
       ]  