delay :: Integer -> Integer
delay 0 = 0
delay n = delay$ (n-1)

baseDelay = 10^6
main = print $delay $ 300*baseDelay