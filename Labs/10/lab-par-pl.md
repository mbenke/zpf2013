# N queens

Napisz funkcję rozmieszczającą n hetmanów na szachownicy n*n

* sekwencyjnie

* równolegle

~~~~ {.haskell}
type PartialSolution = [Int]
type Solution = PartialSolution
type BoardSize = Int

queens :: BoardSize -> [Solution]
queens n = iterate (concatMap (addQueen n)) [[ ]] !! n

addQueen :: BoardSize -> PartialSolution -> [PartialSolution]
addQueen n s = [x : s | x <- [1..n], safe x s 1]

safe :: Int -> PartialSolution -> Int -> Bool
safe x [] n = True
safe x (c : y) n = x /= c && x /= c + n 
       && x /= c - n && safe x y (n + 1)
~~~~

# Liczby Fibonacciego

~~~~ {.haskell}
cutoff :: Int
cutoff = 20

parFib n | n < cutoff = fib n
parFib n = p `par` q `pseq` (p + q)
    where
      p = parFib $ n - 1
      q = parFib $ n - 2

fib n | n<2 = n
fib n = fib (n - 1) + fib (n - 2)
~~~~

* Przepisz powyższy program uzywając monady `Eval`

* Sprawdź jakie wartości cutoff będą dobre dla róznych stopni równoległosci N

* Wypróbuj inne strategie obliczeń

# Co by tu jeszcze zrównoleglić

* Spróbuj zrównloleglic jakiś własny przykład (liczby pierwsze? sortowanie? ...?)