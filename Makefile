# $Id: Makefile#6 2009/04/22 10:12:37 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for ThreadScope
# Maintainer: satnams@microsoft.com


cabal:	
	cabal install --user --prefix=$(HOME)/haskell

haddock:
	cabal haddock --executables