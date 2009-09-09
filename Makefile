# $Id: Makefile#7 2009/07/18 22:48:30 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for ThreadScope
# Maintainer: satnams@microsoft.com

GHC = /c/ghc/ghc-6.10.3/bin/ghc

cabal:	
	cabal install -w $(GHC) --user --prefix=$(HOME)/haskell

haddock:
	cabal haddock --executables