#!/bin/sh

HC=$1

set -ex

CABALPKG="cabal-c92b4ea7ce036fae6ebf3c2965d6ecc0ef252775-20170725-123913.xz"
CABALCHECKSUM="2aa74ff75ee97745eb562360ed4e8f95f3eba4ce40c8621b6b23e29633f6ed3a"

GHCPKG="ghc-8.2.1-x86_64-apple-darwin.tar.xz"
GHCURL="https://downloads.haskell.org/~ghc/8.2.1/$GHCPKG"
GHCCHECKSUM="900c802025fb630060dbd30f9738e5d107a4ca5a50d5c1262cd3e69fe4467188"

if [ $(uname) != "Darwin" ]; then
    exit 0
fi

if [ "x$HC" != "xghc-8.2.1" ]; then
    echo "Only GHC-8.2.1 is supported at the moment"
    exit 1
fi

ROOTDIR=$(pwd)
BUILDDIR=$(mktemp -d /tmp/build-cabal-nightly.XXXXXX)

travis_retry () {
    $*  || (sleep 1 && $*) || (sleep 2 && $*)
}

if [ ! -f $HOME/.ghc-install/bin/ghc-8.2.1 ]; then
    cd $BUILDDIR

    travis_retry curl -OL $GHCURL
    # Two spaces seems to be important
    echo "$GHCCHECKSUM  ./$GHCPKG" | shasum -c -a 256

    tar -xJf $GHCPKG
    cd ghc-*
    ./configure --prefix=$HOME/.ghc-install
    make install
fi

if [ ! -f $HOME/.ghc-install/bin/cabal ]; then
    cd $BUILDDIR

    travis_retry curl -OL https://haskell.futurice.com/files/$CABALPKG
    echo "$CABALCHECKSUM  ./$CABALPKG" | shasum -c -a 256

    # gunzip knows how to handle .xz
    gunzip -c $CABALPKG > $HOME/.ghc-install/bin/cabal
    mkdir -p $HOME/.ghc-install/bin
    chmod a+x $HOME/.ghc-install/bin/cabal
fi
