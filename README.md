# ThreadScope
[![Hackage](https://img.shields.io/hackage/v/threadscope.svg)](https://hackage.haskell.org/package/threadscope)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/threadscope.svg)](http://packdeps.haskellers.com/feed?needle=threadscope)
[![Build Status](https://travis-ci.org/haskell/ThreadScope.svg?branch=master)](https://travis-ci.org/haskell/ThreadScope)
[![Build status](https://ci.appveyor.com/api/projects/status/tiwkb7k6p38dde03/branch/master?svg=true)](https://ci.appveyor.com/project/maoe/threadscope-44t6e/branch/master)

## Using pre-built binaries

Currently [pre-built binaries](https://github.com/haskell/ThreadScope/releases) for the following platforms are provided:

* Ubuntu Trusty (64-bit)
* OS X
* Windows (x64)

GTK+2 needs to be installed for those binaries to work.

On OS X, `gtk-mac-integration` also needs to be installed.
```
brew install gtk+ gtk-mac-integration
```

On Windows, the [MSYS2](http://www.msys2.org) is the recommended way to install GTK+2. In MSYS2 MINGW64 shell:
```
pacman -S $MINGW_PACKAGE_PREFIX-gtk2
```
then you can run the threadscope binary from the shell.

## Building from source

### Linux

GTK+2 is required to be installed. On Ubuntu-like systems:

```sh
sudo apt install libgtk2.0-dev
```

Then you can build threadscope using cabal:
```sh
cabal new-build
```

Or using stack:
```sh
stack setup
stack install
```

### OS X

GTK+ and gtk-mac-integration are required.

```sh
brew install gtk+ gtk-mac-integration
```

Then you can build threadscope using cabal:
```sh
cabal new-build --constraint="gtk +have-quartz-gtk"
```

Or using stack:
```sh
stack setup
stack install --flag gtk:have-quartz-gtk
```

### Windows

stack is the recommended tool to build threadscope on Windows.

CAVEAT: Currently gtk2 needs to be installed twice: one for stack's MSYS2 environment and another for local MSYS2 environment.

In command prompt:
```sh
stack setup
stack exec -- pacman --needed -Sy bash pacman pacman-mirrors msys2-runtime msys2-runtime-devel
stack exec -- pacman -Syu
stack exec -- pacman -Syuu
stack exec -- pacman -S base-devel mingw-w64-x86_64-pkg-config mingw-w64-x86_64-toolchain mingw-w64-x86_64-gtk2
stack install
```

Then in MSYS2 MINGW64 shell:
```sh
pacman -S $MINGW_PACKAGE_PREFIX-gtk2
echo 'export PATH=$APPDATA/local/bin:$PATH' >> .profile
source .profile
threadscope
```
