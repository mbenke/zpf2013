BINFILES=sudoku1 sudoku2 sudoku2b sudoku3 sudoku3b fork \
  geturls1 geturls2 sudoku-par2 sudoku-par3
GHCOPTS=-O2 -rtsopts -threaded -eventlog

%: %.hs
	ghc $(GHCOPTS) -o $@ --make $^

all: $(BINFILES)

#sudoku1: sudoku1.hs
#	ghc $(GHCOPTS) -o $@ --make $^


clean:
	-rm -f *.hi *.o *~

veryclean: clean
	-rm -f $(BINFILES)

.PHONY: clean veryclean
