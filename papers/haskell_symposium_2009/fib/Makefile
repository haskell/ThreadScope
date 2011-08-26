GHC = /c/ghc/ghc/inplace/bin/ghc-stage2
GHC_OPTS = -threaded -eventlog

all:	
	$(GHC) $(GHC_OPTS) --make Fib1.hs
	$(GHC) $(GHC_OPTS) --make Fib2.hs

run1:	
	./Fib1 +RTS -N1 -qg0 -qb 
	./Fib1 +RTS -N2 -qg0 -qb

run2:	
	./Fib2 +RTS -N1 -qg0 -qb -H50M
	./Fib2 +RTS -N2 -qg0 -qb -H500M

clean:	
	rm -rf *.hi *.o