include tex.mk
#MDFILE := $(word 1, $(basename $(wildcard *.md)))
MDFILE = lenses
MATHJAXURL='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
L ?= $(MDFILE)
MATH=--mathjax #=$(MATHJAXURL)
#MATH=--mathml
PDOPTS=$(MATH) #--self-contained
#PANDOC=~/.cabal/bin/pandoc $(PDOPTS)
PANDOC=pandoc $(PDOPTS)
DESTDIR=.
GENERATED := $(L).html $(L)-slides.html $(L)-slides.tex *.pdf

all: $(L).html $(L)-slides.html $(L)-slides.tex
.PHONY: all echo

#$(L).html: $(L).md
#	@test -f <
#	$(PANDOC) -s -t html --mathjax -o $(DESTDIR)/$@ $<
#
#$(L)-slides.html: $(L).md Makefile $(wildcard ./pandoc/slidy/*)
#	@test -f $<
#	$(PANDOC) -s -t slidy -o $(DESTDIR)/$@ $<

$(L).html: $(L).lhs
	@test -f $<
	$(PANDOC) -s -f markdown+lhs -t html --mathjax -o $(DESTDIR)/$@ $<

$(L)-slides.html: $(L).lhs Makefile $(wildcard ./pandoc/slidy/*)
	@test -f $<
	$(PANDOC) -s -f markdown+lhs -t slidy -o $(DESTDIR)/$@ $<

$(L)-slides.tex: $(L).lhs
	@test -f $<
	$(PANDOC) -s -f markdown+lhs -t beamer --mathjax -o $(DESTDIR)/$@ $<

echo: 
	echo $(MDFILE)

distclean: clean
	-rm -f $(GENERATED)