# Changelog

## Unreleased

### Added
- Bug triage and prioritization document
  - see `docs/triage.md`

## [0.5.2](https://github.com/actonlang/acton/releases/tag/v0.5.2) (2021-08-25)

### Fixed
- It is now possible to raise exceptions.
  - Previously a naming misalignment between the Acton compiler code generation
    and the builtins of the RTS led to an Internal Compiler Error.
  - BaseException is now also used instead of Exception, as it should.
- Distributed RTS mode has been fixed with regards to actor bootstrap that
  previously lead to duplicate actors.
- An actor method (callback) can now be passed as an argument to another actor
  method
  - This could previously lead to a segmentation fault during certain
    situations.
- Avoid cleaning away `modules/math.h`
  - This was due to an outdated Makefile cleaning target
- Type inferencing now works for all dicts.
  - Previously worked for some, for example a dict of strings but not for a dict
    of ints.
- The modules `acton.rts`, `math` and `random` now work correctly
  - The type signature filed was missing but is now correctly built.
  - There are test cases for these modules but the tests were not run in CI
- All tests are now run in CI
  - Due to a directory restructuring and assumptions (mother of all evil), some
    tests were previously not run. While working locally on a developers
    computer things worked due to manually compiled files. We failed to detect
    these missing type signature files in CI since the tests were not run
    automatically.
- Type inference is now order independent.
  - There were previously situations in which the type inferencer was unable to
    do its job based on the layout / order of the code.
- `print(foo())` now correctly prints the return value of foo() rather than a
  message reference (due to asynchronous evaluation).


## [0.5.1](https://github.com/actonlang/acton/releases/tag/v0.5.1) (2021-08-24)

### Fixed
- v0.5.0 was released with an incorrect version number and would identify itself
  as version v0.4.2

## [0.5.0](https://github.com/actonlang/acton/releases/tag/v0.5.0) (2021-08-23)

### Added
- The distributed database backend is enabled in the RTS
  - this was previously a build time flag
  - it is now run time configuration via `--rts-ddb-host` and `--rts-ddb-port`
  - see `examples/count` for a demo and instructions
- RTS now has a `--rts-verbose` argument to make it more chatty
- General evaluation of expressions in a boolean context
  - explicit boolean expressions were already supported and working
  - however, in Python any expression can be evaluated in a boolean context
    which was not properly supported... and it now is!
  - this could be considered a bug fix, like it should go under "Fixed" but it
    is big enough of a thing to warrant being here... almost like new
    functionality, although we probably did intend to support this all along
- Description of development workflow, see `docs/development_workflow.md`
- A short guide on how to wrap C libraries for use in Acton, see
  `docs/wrapping_c_libraries.md`
- `time.time()` function
  - returns the current system time in nanoseconds as an integer
  - `time` is a new module
- `random.randint()` function
  - `random` is a new module
- `acton.rts.sleep()` function
  - from a user perspective acts just like Pythons `time.sleep()`
  - it uses `usleep` in C which means it actually sleeps the RTS thread
    executing the actor
    - this is typically a bad thing - we just want to sleep an actor, not the
      RTS thread
    - there are however use cases, like when implementing benchmarking or
      similar.
    - this is why it is under `acton.rts` and not in `time`
  - `acton.rts` is a new module

### Changed
- Refactored Makefile structure to complete DAG
  - use a single top level Makefile rather than recursive Makefiles
  - only matters if you are doing development on Acton itself
  - backend, being enough of a standalone piece, still has its own Makefile
- `actonc --hgen` and `actonc --cgen` now outputs only code
  - easier to pipe directly to a file
  - compilation command not included, now available with `actonc --ccmd`
- `actonc` argument `--path` is now called `--syspath`
  - it is the system path and was already referred to as `syspath` internally
- The build output, when building Acton itself, is now placed in `dist`
  - makes it considerably easier to produce a release by tarring up its content
  - release size has been reduced to almost half by avoiding unnecessary files
  - the top level directories, like `modules` is now a source directory and no
    longer "the working directory of `actonc`"
- Source code of several modules have been moved from separate subdirs, like
  `math` and `time`, into `modules/`. Less top level clutter!

### Fixed
- Improved dependency declaration for backend
  - Only matters if you are building Acton yourself and making changes to the
    backend database

## [0.4.2](https://github.com/actonlang/acton/releases/tag/v0.4.2) (2021-08-05)

### Added
- `tip` releases are now available at stable URLs:
  - https://github.com/actonlang/acton/releases/download/tip/acton-darwin-x86_64.tar.bz2
  - https://github.com/actonlang/acton/releases/download/tip/acton-linux-x86_64.tar.bz2

### Fixed
- Versioned releases now use correct version number, like `0.4.1` rather than
  the version style used for `tip` releases which include the date & time, like
  `0.4.1.2021.08.05.12.15.37`
- tip release tar balls now reflect the full tip version in the filename
  - like `acton-linux-x86_64-0.4.1.20210805.13.46.55.tar.bz2`
  - previously, it would just be `acton-linux-x86_64-0.4.1.tar.bz2`, making it
    difficult to differentiate against the proper 0.4.1 or other nightlies


## [0.4.1](https://github.com/actonlang/acton/releases/tag/v0.4.1) (2021-08-05)

### Added
- `--version --verbose` will print verbose version information
- Internal compiler errors now include information about the C compiler (cc)
- Acton is now made available as GitHub Releases
  - pre-built binary releases! Users no longer need to compile Acton themselves
  - available from [Releases page](https://github.com/actonlang/acton/releases)
  - versioned releases, like this v0.4.1, are available
  - latest build from `main` branch is available as `tip`

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
