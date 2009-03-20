# $Id: Makefile#1 2009/03/20 13:27:50 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for eventlog_viewer
# Maintainer: satnams@microsoft.com

EVENTLOG_FILES = $(HOME)/haskell/ghc-profiling/Events

all:	
	ghc --make ThreadScope.hs -i$(EVENTLOG_FILES) -o threadscope

cabal:	
	runhaskell Setup configure --prefix=$(HOME)/haskell --user
	runhaskell Setup build
	runhaskell Setup install

install:	
	cp threadscope.exe threadscope.glade /c/bin

tarball:	
	tar cvf threadscope.tar Makefile *.hs threadscope.glade GHC/RTS/EventLogFormat.h GHC/RTS/Events.hs LICENSE

clean:	
	rm -rf *.hi *.exe *.o *.manifest