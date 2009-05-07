GHC = /c/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -threaded -eventlog
# HEAP = -H100M
HEAP =

all:	
	$(GHC) $(GHC_OPTS) --make SuMEuler0.hs
	$(GHC) $(GHC_OPTS) --make SumEuler1.hs
	$(GHC) $(GHC_OPTS) --make SumEuler2.hs
	$(GHC) $(GHC_OPTS) --make SumEuler3.hs


run:	run0 run1 run2 run3

run0:	
	./SumEuler0

run1:	
	./SumEuler1 +RTS -N1 -qg0 -qb $(HEAP) -l -sSumEuler1.N1.log
	mv SumEuler1.exe.eventlog SumEuler1.N1.eventlog 
	./SumEuler1 +RTS -N2 -qg0 -qb $(HEAP) -l -sSumEuler1.N2.log
	mv SumEuler1.exe.eventlog SumEuler1.N2.eventlog 

run2:	
	./SumEuler2 +RTS -N1 -qg0 -qb $(HEAP) -l -sSumEuler2.N1.log
	mv SumEuler2.exe.eventlog SumEuler2.N1.eventlog 
	./SumEuler2 +RTS -N2 -qg0 -qb $(HEAP) -l -sSumEuler2.N2.log
	mv SumEuler2.exe.eventlog SumEuler2.N2.eventlog

run3:	
	./SumEuler3 +RTS -N1 -qg0 -qb $(HEAP) -l -sSumEuler3.N1.log
	mv SumEuler3.exe.eventlog SumEuler3.N1.eventlog 
	./SumEuler3 +RTS -N2 -qg0 -qb $(HEAP) -l -sSumEuler3.N2.log
	mv SumEuler3.exe.eventlog SumEuler3.N2.eventlog 

clean:	
		rm -rf *.hi *.o

cleanall:	clean
		rm -rf *.eventlog *.log