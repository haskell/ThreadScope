# $Id: Makefile#4 2009/03/23 17:39:00 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for eventlog_viewer
# Maintainer: satnams@microsoft.com


cabal:	
	runhaskell Setup configure --prefix=$(HOME)/haskell --user
	runhaskell Setup build
	runhaskell Setup install

