GHC = /c/ghc/ghc/ghc/stage2-inplace/ghc
GHC_OPTS = -O -threaded -eventlog

all:	
	$(GHC) $(GHC_OPTS) --make Null.hs
	$(GHC) $(GHC_OPTS) --make Hello.hs


rnull:	
	./Null +RTS -l

rhello:	
	./Hello +RTS -l

clean:	
	rm -rf *.o *.hi *.exe