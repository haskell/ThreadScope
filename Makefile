# Makefile for ThreadScope

GHC = c:/ghc/ghc-6.10.3/bin/ghc

cabal:
	cabal install -w $(GHC) --user --prefix=$(HOME)/haskell


sdist:
	cabal sdist

haddock:
	cabal haddock --executables

clean:
	cabal clean
