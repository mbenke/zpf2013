BINFILES=pmparsec pm2a pm2c pm3a
GHCOPTS=-O -rtsopts

all: $(BINFILES)

pmparsec: PlusMinusParsec.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm2a: PlusMinus2a.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm2c: PlusMinus2c.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm3a: PlusMinus3a.hs
	ghc $(GHCOPTS) -o $@ --make $^

clean:
	-rm -f *.hi *.o

veryclean: clean
	-rm -f $(BINFILES)