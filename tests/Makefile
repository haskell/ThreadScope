GHC = /c/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -O -threaded -eventlog

all:	
	$(GHC) $(GHC_OPTS) --make Null.hs
	$(GHC) $(GHC_OPTS) --make Hello.hs
	$(GHC) $(GHC_OPTS) --make SumEulerPar1.hs
	$(GHC) -threaded -eventlog --make ParFib.hs


rnull:	
	./Null +RTS -l

rhello:	
	./Hello +RTS -l

rsep1:	
	./SumEulerPar1 +RTS -l -N8

rparfib:
	./ParFib +RTS -l -N2

run:	rnull rhello rsep1

clean:	
	rm -rf *.o *.hi *.exe