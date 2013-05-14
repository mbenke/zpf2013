BINFILES=fork geturls geturls1 geturls2 Par1\
 IORef1 IORef2 IORef3 IORef4 stm1

GHCOPTS=-O2 -rtsopts -threaded

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
