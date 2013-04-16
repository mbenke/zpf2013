BINFILES=declare1 declare2 declare3

all: $(BINFILES)

declare1: Declare1.hs Build1.hs
	ghc -o $@ --make $^

declare2: Declare2.hs Build2.hs
	ghc -o $@ --make $^

declare3: Declare3.hs Build3.hs
	ghc -o $@ --make Declare3.hs

clean:
	-rm -f *.hi *.o $(BINFILES)