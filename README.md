## Materiały do wykładu "Zaawansowane Programowanie Funkcyjne" Wydział MIM UW

## "Advanced Functional Programming" course materials  (in Polish)

* Gotowe notatki/slajdy w katalogu www
* Kod w katalogu Code
* Do wygenerowania notatek i slajdów ze źródeł potrzebny program pandoc

### Szybki start

~~~~~
$ cabal install pandoc
$ PATH=~/.cabal/bin:$PATH            # Linux
$ PATH=~/Library/Haskell/bin:$PATH   # OS X
$ git clone git://github.com/mbenke/zpf2013.git
$ cd zpf2013/Slides
$ make
~~~~~