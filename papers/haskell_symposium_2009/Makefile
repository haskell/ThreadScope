# $Id: Makefile#2 2009/04/29 17:03:54 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/papers/haskell_symposium_2009/Makefile $

DOC = ghc-parallel-tuning

all:	
	pdflatex $(DOC).tex
	bibtex $(DOC)
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

spell:	
	aspell -c ghc-parallel-tuning.tex
	aspell -c motivation.tex
	aspell -c threadring.tex
	aspell -c bsort.tex
	aspell -c related-work.tex

clean:
	rm -rf *.bbl *.blg *.log *.aux *.dvi