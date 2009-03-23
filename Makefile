# $Id: Makefile#3 2009/03/23 17:11:32 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ThreadScope/Makefile $

# Makefile for eventlog_viewer
# Maintainer: satnams@microsoft.com

EVENTLOG_FILES = $(HOME)/haskell/ghc-events

all:	
	ghc --make ThreadScope.hs -i$(EVENTLOG_FILES) -o threadscope

cabal:	
	runhaskell Setup configure --prefix=$(HOME)/haskell --user
	runhaskell Setup build
	runhaskell Setup install



tarball:	
	tar cvf threadscope.tar Makefile *.hs threadscope.glade GHC/RTS/EventLogFormat.h GHC/RTS/Events.hs LICENSE

clean:	
	rm -rf *.hi *.exe *.o *.manifest