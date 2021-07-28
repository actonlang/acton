# Changelog

## Unreleased

### Added
- `--version --verbose` will print verbose version information
- internal compiler errors now include information about the C compiler (cc)

### Fixed
- integer subtraction has been fixed
  - wrong order of operators would previously return wildly incorrect results


## [0.4.0](https://github.com/actonlang/acton/releases/tag/v0.4.0) (2021-07-23)

### Added
- Linux compatibility!
  - it is now possible to compile the Acton compiler (actonc), the backend and
    the RTS on Linux
  - actonc can now compile Acton programs on Linux
  - Linux on x86_64 is tested, no other architectures
  - libkqueue is used on Linux as a compatibility shim between kqueue calls and
    epoll
    - Acton was initially developed on OS X and the RTS therefore uses kqueue
      and not epoll / aio(uring)
- More test cases, in particular the ones for which we have GitHub issues opened
- CI runs on Linux too
  - acton is built and tested on both Linux and OS X
  - all tests are the same across both platforms
- `actonc` has got a `--version` argument

### Changed
- Now uses Haskell lts-17.14 instead of lts-13.0

### Removed
- The project repository has been cleaned up overall

### Fixed
- `actonc` now detects and reports internal compiler errors
  - happens when gcc fails to compile the C code that actonc generates
- A bunch of compiler surprises have been made less surprising
  - see git log for details ;)


## [0.3.0](https://github.com/actonlang/acton/releases/tag/v0.3.0) (2021-04-14)

### Added
- Acton is now public and published at its new [home on
  Github](https://github.com/actonlang/acton/)
- Added basic (working) build instructions
- A basic test suite with a few tests, including some of the examples being
  reused for testing
- GitHub Actions is used as CI to build and test Acton
  - Mac OS X / Darwin is the only platform used for testing Acton

### Changed
- distributed database backend is disabled in builds per default
  - while working, it is too rough around the edges to be enabled per default
  - in particular, it needs to be made run time configurable


## 0.2.0 (a long long time ago, like 2019)

This is a sort of fictitious release that roughly maps to the prehistoric era
before recorded time. There was a git log but no real version keeping.

0.2.0 was the first ever version of the "new compiler". It wasn't 0.1 because in
an earlier prehistoric era, another system existed which could be called the
first incarnation of Acton. It was more of a proof of concept based on Python as
a runtime and with a hacked up Cassandra database as a backend store. Since
then, this second incarnation has been in focus and 0.2.0 was its first version.
