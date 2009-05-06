GHC = /c/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -threaded -eventlog

all:	
	$(GHC) $(GHC_OPTS) --make SumEuler1.hs
	$(GHC) $(GHC_OPTS) --make SumEuler2.hs

run1:	
	./SumEuler1 +RTS -N1 -qg0 -qb -H100M -l
	mv SumEuler1.exe.eventlog SumEuler1.N1.eventlog 
	./SumEuler1 +RTS -N2 -qg0 -qb -H100M -l
	mv SumEuler1.exe.eventlog SumEuler1.N2.eventlog 

run2:	
	./SumEuler2 +RTS -N1 -H100M -l
	mv SumEuler2.exe.eventlog SumEuler2.N1.eventlog 
	./SumEuler2 +RTS -N2 -H100M -l
	mv SumEuler2.exe.eventlog SumEuler2.N2.eventlog 

clean:	
	rm -rf *.hi *.o