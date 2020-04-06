# Revision history for threadscope

## 2020-04-06 - v0.2.13

* Add changelog to extra-source-files ([#105](https://github.com/haskell/ThreadScope/pull/105))
* Fix broken GitHub Releases deployment ([#106](https://github.com/haskell/ThreadScope/pull/106))
* Update ghc-events to 0.13.0 ([#107](https://github.com/haskell/ThreadScope/pull/107))
* Relax upper version bound for time

## 2020-03-04 - v0.2.12

* Remove unused events entry box ([#93](https://github.com/haskell/ThreadScope/pull/93))
* Make the app work even if it fails to load the logo ([#96](https://github.com/haskell/ThreadScope/pull/96))
* Support GHC 8.8 ([#99](https://github.com/haskell/ThreadScope/pull/99))
* Support ghc-events 0.12.0 ([#101](https://github.com/haskell/ThreadScope/pull/101))
* Stop using gtk-mac-integration and fix broken CI ([#103](https://github.com/haskell/ThreadScope/pull/103))
  * This causes a visual regression. The logo won't be displayed in Dock.

## 2018-07-12 - v0.2.11.1

* Relax upper version bounds for containers and ghc-events (#88)

## 2018-06-08 - v0.2.11

* Relax upper version bounds for template-haskell and temporary
* Fix build failure with gtk-0.14.9
* Modernise AppVeyor CI script

## 2018-02-16 - v0.2.10

* Add instructions to install gtk2 in the README
* Do not include windows_cconv.h on non mingw32 systems (#79)
* Relax upper version bound for ghc-events (#80)
* Relax upper version bound for time

## 2017-09-02 - v0.2.9

* Render GC waiting periods in light orange (#70)
* Fix inappropriate calling convention on Windows x86 (#71)
* Enable GitHub Releases (#75)

## 2017-07-17 - v0.2.8

* Add macOS support (#56)
* Update ghc-events to 0.6.0 (#61)
* CI builds for Linux/Windows/macOS (#64, #65)
* Set upper version bounds for dependencies
