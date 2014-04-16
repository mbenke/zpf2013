PDFLATEX=pdflatex -interaction=batchmode
%.pdf: %.tex
	$(PDFLATEX) $<; $(PDFLATEX) $<

clean:: texclean

texclean::
	-rm -f *.aux *.log *.toc *.out *.snm *.nav *.vrb *.dvi *.synctex.gz
