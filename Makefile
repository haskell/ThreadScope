# $Id: Makefile#5 2009/03/30 13:46:44 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for ThreadScope
# Maintainer: satnams@microsoft.com


cabal:	
	cabal install --user

haddock:
	cabal haddock --executables