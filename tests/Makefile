GHC = c:/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -O -threaded -eventlog

all:	
	$(GHC) $(GHC_OPTS) --make Null.hs
	$(GHC) $(GHC_OPTS) --make Hello.hs
	$(GHC) $(GHC_OPTS) --make SumEulerPar1.hs
	$(GHC) -threaded -eventlog --make ParFib.hs

run:	cleanlogs rnull rhello rsep1 rparfib

rnull:	
	./Null +RTS -ls

rhello:	
	./Hello +RTS -ls

rsep1:	
	./SumEulerPar1 +RTS -ls -N8

rparfib:
	./ParFib +RTS -ls -N2

cleanlogs:	
		rm -rf *.eventlog

clean:	
	rm -rf *.o *.hi *.exe *.eventlog