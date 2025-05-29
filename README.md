# ThreadScope

[![Hackage](https://img.shields.io/hackage/v/threadscope.svg)](https://hackage.haskell.org/package/threadscope)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/threadscope.svg)](http://packdeps.haskellers.com/feed?needle=threadscope)
![CI](https://github.com/haskell/ThreadScope/workflows/CI/badge.svg?branch=master)

## Using pre-built binaries

Currently [pre-built binaries](https://github.com/haskell/ThreadScope/releases) for the following platforms are provided:

* Ubuntu 24.04 (64-bit)
* macOS 14.7
* Windows Server 2022 (x64)

GTK+3 needs to be installed for these binaries to work.

On Windows, the [MSYS2](http://www.msys2.org) is the recommended way to install GTK+3. In MSYS2 MINGW64 shell:

```sh
pacman -S $MINGW_PACKAGE_PREFIX-gtk3
```

then you can run the threadscope binary from the shell.

## Building from source

Use `git clone` or `cabal get threadscope` to get the source and move into the threadscope directory.

The code for the Github Actions is a good guide for building from source.

### Linux

GTK+3 is required to be installed. On Ubuntu-like systems:

```sh
sudo apt install libgtk-3-dev
```

Then you can build threadscope using cabal:

```sh
cabal v2-build   # to only build the project, or
cabal v2-install # to build and install the binary
```

Or using stack:

```sh
stack build   # to only build the project, or
stack install # to build and install the binary
```

### macOS

GTK+ is required:

```sh
brew install cairo gtk+3 pkg-config
```

Then you can build threadscope using cabal:

```sh
cabal --project-file=cabal.project.osx v2-build   # to only build the project, or
cabal --project-file=cabal.project.osx v2-install # to build and install the binary
```

Or using stack:

```sh
stack --stack-yaml=stack.osx.yaml build   # to only build the project, or
stack --stack-yaml=stack.osx.yaml install # to install the binary
```

### Windows

> [!CAUTION]
> The Windows instructions may be out of date. Contributions to update them would be welcome.

[Chocolatey](https://chocolatey.org/) can be used to install GHC and [MSYS2](https://www.msys2.org/) is the recommended way to install GTK+.

```sh
choco install ghc
refreshenv
set PATH=C:\\msys64\\mingw64\\bin;C:\\msys64\\usr\\bin;%PATH%
pacman -Sy mingw-w64-x86_64-gtk3
```

then you can build threadscope using cabal:

```sh
cabal v2-build
```

Or you can use stack instead.

CAVEAT: gtk3 needs to be installed twice: one for stack's MSYS2 environment and another for local MSYS2 environment.

In command prompt:

```sh
stack setup
stack exec -- pacman --needed -Sy bash pacman pacman-mirrors msys2-runtime msys2-runtime-devel
stack exec -- pacman -Syu
stack exec -- pacman -Syuu
stack exec -- pacman -S base-devel mingw-w64-x86_64-pkg-config mingw-w64-x86_64-toolchain mingw-w64-x86_64-gtk3
stack install
```

Then in MSYS2 MINGW64 shell:

```sh
pacman -S $MINGW_PACKAGE_PREFIX-gtk3
echo 'export PATH=$APPDATA/local/bin:$PATH' >> .profile
source .profile
threadscope
```

Building using stack is not tested in CI. If you find any issues with building with stack, please update the instructions and send a PR.
