BINFILES=TestReg
PDFLATEX=pdflatex -interaction=batchmode

test: TestReg
	./TestReg

TestReg: TestReg.hs Reg.hs RegExtra.hs Mon.hs
	ghc -O2 --make $@

%.tex: %.md
	pandoc -t latex -H pl.sty -sS $^ > $@

%.pdf: %.tex
	$(PDFLATEX) $<; $(PDFLATEX) $<

%.moo: %.md
	sed 's/{.haskell}//g' < $^ > $@

clean:: texclean hsclean
	-rm -f *~

hsclean::
	-rm -f *.hi *.o $(BINFILES)

texclean::
	-rm -f *.aux *.log *.toc *.out *.snm *.nav *.vrb *.dvi
