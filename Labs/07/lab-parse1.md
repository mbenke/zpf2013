# Lepsza obsługa błędów

Chcielibyśmy, by komunikat o błędzie podawał: 

* gdzie wystąpił błąd
* czego oczekiwano...

Dla zrealizowania pierwszego postulatu, stan musi przechowywać bieżącą pozycję, np.

~~~~ {.haskell}
data State = State {stPos :: Pos, stInput :: String}
~~~~

* Ćwiczenie: zmodyfikuj MyParsec2a tak, aby przechowywał i raportował
  pozycje błędów.

* Ćwiczenie: zmodyfikuj swoje rozwiązanie poprzeDniego ćwiczenia by działało jak Parsec:

~~~~
Prelude Text.Parsec> parse digit "" ""
Left (line 1, column 1):
unexpected end of input
expecting digit
~~~~

* Ćwiczenie: połączyć pomysły 2c (Empty/Consumed) i 3a (kontynuacje) ewentualnie można jeszcze dołożyć 2b (obsługa błędów).

* Wskazówka:

~~~~ {.haskell}
newtype Parser a = Parser {unParser :: forall b .
                 State
              -> (a -> State -> ParseError -> b) -- consumed ok
              -> (ParseError -> b)               -- consumed err
              -> (a -> State -> ParseError -> b) -- empty ok
              -> (ParseError -> b)               -- empty err
              -> b
             }
~~~~

