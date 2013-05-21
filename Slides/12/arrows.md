# Sygnały i programowanie reaktywne

* Systemy hybrydowe: elementy analogowe (ciągłe) i cyfrowe (dyskretne)

* Sygnały: wartości zmienne w czasie

* Przetworniki sygnałów (SF)

```
Signal a ~ Time -> a
SF a b   ~ Signal a -> Signal b
```

(historycznie: Hallgren, Carlsson 1995: użycie procesorów strumieni
 do programowania GUI w Haskellu)

~~~~ {.haskell}
data SP a b = Put b (SP a b) | Get (a -> SP a b)
~~~~

# Przetworniki

![](YampaArrows.png "Przetworniki")

# Strzałki

* `instance Monad m c`: obliczenie o wyniku typu `c`

* `instance Arrow a b c`: obliczenie przetwarzające `a` na `b`

Pierwsze przybliżenie (Hughes 2000 ``Generalising monads to arrows'')

~~~~ {.haskell}
class Arrow a where
    arr   :: (b->c) ->  a b c
    (>>>) :: a b c -> a c d -> a b d
~~~~

Dla dowolnej monady `m`, funkcje `a -> m b` są dobrymi kandydatami:

~~~~ {.haskell}
newtype Kleisli m a b = K(a -> m b)

instance Monad m => Arrow (Kleisli m) where
  arr f       = K $ return . f
  K f >>> K g = K $ f >=> g

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> (f x >>= g)

~~~~

# Problem

Obliczenie sumujące wyniki dwóch obliczeń

~~~~ {.haskell}
add1 :: Monad m => m Int -> m Int -> m Int 
add1 mx my = mx >>= \x-> my >>= \y -> return $ x+y
-- liftM2 (+)
~~~~

Jak to wyrazić przy pomocy strzałek?

~~~~ {.haskell}
add2 :: Arrow a => a b Int -> a b Int -> a b Int
~~~~

`arr (+)` ?

~~~~ {.haskell}
addInt :: Int -> Int -> Int
addInt = (+)

> :t arr addInt
arr addInt :: Arrow a => a Int (Int -> Int)
~~~~

Bramki z jednym wejściem są za słabe...

# Pomysł

~~~~ {.haskell}
add2 :: Arrow a => a b Int -> a b Int -> a b Int
add2 f g = (?)
~~~~

![](arrAdd.png "arrow add")

Tak `f` jak i `g` potrzebują wejścia typu `b`.

* Rozdzielmy wejście na dwie kopie

* jedną podajmy do `f`, drugą zachowajmy

* zachowajmy wynik `f`, podając drugą kopię do `g`

* zsumujmy wyniki

# Demultiplekser

Zamiast dwóch wejść, możemy uzyc jednego, które jest parą...

...pod warunkiem, że potrafimy operować na składnikach niezależnie

~~~~ {.haskell}
instance Arrow a where
    first :: a b c -> a (b,d) (c,d)
~~~~

![](first.png "first")

~~~~ {.haskell}
    arr :: (b -> c) -> a b c
~~~~

![](arr.png "arr")

~~~~ {.haskell}
(>>>) :: a b c -> a c d -> a b d
~~~~

![](compose.png "compose")

# second?

Mamy `first` a gdzie jest second?

~~~~ {.haskell}
second :: a b c -> a (d, b) (d, c)
second f = arr swap >>> first f >>> arr swap 
  where swap (x,y) = (y,x)
~~~~

![](secondImp.png "second")

# `first f >>> second g`

Teraz możemy przetwarzać obie składowe:

~~~~ {.haskell}
(***)   :: a b c -> a b' c' -> a (b, b') (c, c')
f *** g = first f >>> second g

(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
f &&& g = arr dup >>> (f *** g)
  where dup x = (x,x)
                
add2 :: Arrow a => a b Int -> a b Int -> a b Int
add2 f g = (f &&& g) >>> arr (\(u,v) -> u + v)
                           -- uncurry (+)
~~~~

![](arrAdd.png "arrow add")

# Koniec

~~~~ {.haskell}

~~~~
