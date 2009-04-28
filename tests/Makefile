GHC = /c/ghc/ghc/ghc/stage2-inplace/ghc
GHC_OPTS = -O -threaded -eventlog

all:	comp rnull rhello rse1

comp:	
	$(GHC) $(GHC_OPTS) --make Null.hs
	$(GHC) $(GHC_OPTS) --make Hello.hs
	$(GHC) $(GHC_OPTS) --make SumEulerPar1.hs


rnull:	
	./Null +RTS -l

rhello:	
	./Hello +RTS -l

rse1:	
	./SumEulerPar1 +RTS -N2 -l

clean:	
	rm -rf *.o *.hi *.exe