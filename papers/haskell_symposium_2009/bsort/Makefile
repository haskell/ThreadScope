GHC = /c/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -threaded -eventlog
# HEAP = -H100M
HEAP =
EBH = -feager-blackholing

all:	
	$(GHC) $(GHC_OPTS) --make BSort.hs -O -o bsort
	$(GHC) $(GHC_OPTS) --make BSortPar.hs -O -o bsortpar
	$(GHC) $(GHC_OPTS) --make BSortStreaming.hs -o bsort_streaming


run1:	
	./bsort +RTS -N1 -l -qg0 -qb 

run2:	
	./bsortpar +RTS -N1 -l -qg0 -qb -sbsortpar-N1.log
	mv bsortpar.exe.eventlog bsortpar-N1.eventlog 
	./bsortpar +RTS -N2 -l -qg0 -qb -sbsortpar-N2.log
	mv bsortpar.exe.eventlog bsortpar-N2.eventlog 

run3:	
	./bsort_streaming +RTS -N1 -l -qg0 -qbw -Sbsort-streaming-n1.log
	mv bsort_streaming.exe.eventlog bsort-streaming-n1.eventlog
	./bsort_streaming +RTS -N2 -l -qg0 -qbw -Sbsort-streaming-n2.log
	mv bsort_streaming.exe.eventlog bsort-streaming-n2.eventlog

clean:	
	rm -f bsort bsortpar bsortpar_streaming *.hi *.o