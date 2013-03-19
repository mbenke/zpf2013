# Nieczyste I/O

Moduł Code/IO/Impure.hs dostarcza funkcji

~~~~ {.haskell}
iputchar :: Char -> ()
igetchar :: () -> Char
~~~~

które realizują znakowe I/O na stdin/stdout. Funkcje te są "nieczyste": 
ich obliczenie powoduje efekt uboczny. Przy EOF igetchar daje '\0'.

Używając tych funkcji:

* Napisz program który wypisze ustalony napis (np. "hello" na stdout)
* Napisz program kopiujący stdin na stdout.

# Dialogowe I/O

Moduł Code/Dialogue2.hs implementuje dialogowe IO. Używając go

* Napisz programy z poprzedniego zadania
* Napisz program kopiujący pliki będące jego argumentami

Do uruchomienia dialogu służy funkcja `runDialogue :: Dialogue a -> IO a`

(opcjonalnie) Zaimplementuj dialogowe I/O za pomocą z funkcji z modułu Impure (ewentualnie dopisując dodatkowe "nieczyste" funkcje.

# Kontynuacyjne I/O 
Napisz funkcje realizujące kontynuacyjne I/O przy pomocy dialogowego I/O

~~~~ {.haskell}
type SuccCont a = a -> Dialogue
type FailCont a = IOError -> Dialogue
type Cont a = FailCont -> SuccCont a -> Dialogue

cgetchar :: Cont Char
cputchar :: Char -> Cont ()
~~~~

oraz inne potrzebne funkcje, po czym przepisz programy z poprzedniego
zadania na wersję kontynuacyjną

# Monady

1. Używając funkcji z pierwszego zadania stwórz monadę własną monadę I/O: MIO a, z operacjami

    ~~~~ {.haskell}
    mgetchar :: MIO Char
    mputchar :: Char -> MIO ()
    runMIO :: MIO () -> IO ()
    ~~~~

można tu użyć definicji

~~~~ {.haskell}
type IO a = World -> (a, World)
~~~~

pierwsze ćwiczenie powinno podpowiedzieć, czym będzie `World`

2. Napisz monadę DIO implementującą monadyczne IO przy pomocy kontynuacyjnego IO. 
Przepisz programy z poprzedniego zadania tak, by używały DIO.



