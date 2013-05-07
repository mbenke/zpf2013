BINFILES=parfib badfib
GHCOPTS=-O2 -rtsopts -threaded -eventlog

%: %.hs
	ghc $(GHCOPTS) -o $@ --make $^

all: $(BINFILES)

clean:
	-rm -f *.hi *.o *~

veryclean: clean
	-rm -f $(BINFILES)

.PHONY: clean veryclean
