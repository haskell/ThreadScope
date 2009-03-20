# $Id: Makefile#10 2009/03/10 17:52:49 REDMOND\\satnams $
# $Source: //depot/satnams/haskell/ghc-profiling/Events/Makefile $

# Makefile for eventlog_viewer
# Maintainer: satnams@microsoft.com

all:	
	ghc --make ThreadScope.hs -o threadscope

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