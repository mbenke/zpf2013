Dany (w pliku `Reg.hs`) typ danych reprezentujący wyrażenia regularne nad alfabetem c:

~~~~ {.haskell}
data Reg c
  = Lit c           -- jeden znak 
  | Reg c :> Reg c  -- konkatenacja
  | Reg c :| Reg c  -- alternatywa (suma)

  | Many (Reg c)    -- gwiazdka 
  | Eps             -- slowo puste
  | Empty           -- jezyk pusty
  deriving (Eq,Show)    
~~~~

Dla danego wyrażenia r, przez L(r) oznaczamy język tego wyrażenia,
rozumiany w standardowy sposób (w razie wątpliwości prosze
pytać). Mówimy, że r akceptuje słowo w gdy w należy do L(r).
Podobnie mówimy, że wyrażenie akceptuje (albo reprezentuje) język.

Uzupełnij moduł RegExtra o definicje omówione poniżej (ewentualnie zastepując występujące w nim `undefined`).

## Eq

Zdefiniuj instancję klasy `Eq` dla Reg:

~~~~
instance Eq c => Eq (Reg c) where
~~~~

## Monoid

Dana (Mon.hs) klasa reprezentującą monoidy

~~~~ {.haskell}
class Mon m where
  m1 :: m
  (<>) :: m -> m -> m
~~~~

Uzupełnij instancję `instance Mon (Reg c)` tak, aby dla dowolnych `x y z` spełnione były własności

~~~~ {.haskell}
leftUnit x = m1 <> x == x
rightUnit x =  x <> m1 == x
assoc x y z = (x<>y)<>z == x<>(y<>z)
~~~~

(mówimy że np. własność `assoc` jest spełniona jesli dla każdych `x y z` odpowiedniego typu `assoc x y z` daje wartość `True`)

Uwaga: udostępniamy program `TestReg.hs`, który po skompilowaniu i uruchomieniu testuje wymagane własności.

## Słowo puste i język pusty

Napisz funkcje

~~~~ {.haskell}
nullable, empty :: Reg c -> Bool
~~~~

takie, że

* `nullable r == True` gdy słowo puste należy do języka
* `empty r == True` gdy język jest pusty

Testy:

~~~~ {.haskell}
nullableUnit = nullable m1
nullableOp x y = nullable x && nullable y ==> nullable (x <> y)
~~~~

## Upraszczanie

Wyrażenia regularne $r_1$ i $r_2$ nazywamy *równoważnymi* jeśli akceptują one ten sam język.
Dla danego wyrażenia regularnego nierzadko możemy podać prostsze wyrażenie 
równoważne, np  `Eps :> (Lit 0 :| Empty)` jest równoważne `Lit 0`

Napisz funkcję

~~~~ {.haskell}
simpl :: Eq c => Reg c -> Reg c
~~~~

Testy:

~~~~ {.haskell}
nullableSimpl x = nullable x `iff` nullable (simpl x)
emptySimpl x = empty x `iff` empty (simpl x)
~~~~ 

dającą wyrażenie równoważne argumentowi a prostsze (w jakimś sensie).
Niektóre potrzebne uproszczenia ujawnią się w późniejszych etapach

## Pochodne

Pochodną języka $L$ względem $c$ jest język zawierający słowa $w$ takie, że 
$cw$ należy do $L$. Pochodna języka regularnego jest zawsze językiem regularnym. 

Napisz funkcje

~~~~ {.haskell}
der :: Eq c => c -> Reg c -> Reg c
ders :: Eq c => [c] -> Reg c -> Reg c
~~~~

dające pochodną wyrażenia regularnego względem (odpowiednio) jednego
znaku i ciągu znaków.

Uwaga: nie od rzeczy może być tu wykorzystanie funkcji `simpl`. 
Jaki rozmiar ma `ders (replicate 1000 A) (Many (Lit A) :> Lit B)` ?

## Dopasowania

Łatwo zauważyć, że słowo należy do języka wtw gdy 
pochodna języka względem tego słowa jest niepusta. 
Wykorzystując ten fakt i funkcje opisane powyzej, napisz funkcje

~~~~ {.haskell}
accepts :: Eq c => Reg c -> [c] -> Bool
mayStart :: Eq c => c -> Reg c -> Bool
match :: Eq c => Reg c -> [c] -> Maybe [c]
search :: Eq c => Reg c -> [c] -> Maybe [c]
findall :: Eq c => Reg c -> [c] -> [[c]]
~~~~

takie, że

* `accepts r w` daje `True` gdy w należy do $L(r)$.
* `mayStart c r` daje `True` gdy $L(r)$ zawiera słowo zaczynające się od $c$
* `match r w` daje `Just p`, gdzie $p$ to najdłuższy prefiks $w$ należacy do $L(r)$, `Nothing` gdy nie ma takiego.
* `search r w` daje `Just u` gdzie $u$ to pierwsze (najdłuższe) podsłowo $w$ akceptowane przez $r$, `Nothing` gdy nie ma takiego.
* `findall r w` daje listę wszystkich (lokalnie najdłuższych) podsłów $w$ pasujących do $r$.