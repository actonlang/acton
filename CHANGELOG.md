# Changelog

## Unreleased

There are currently known regressions:
- timed queue (using `after X:` statements) is not reliable
- using RTS together with the distributed backend database is not working

### Added
- **Acton project** concept [#140](https://github.com/actonlang/acton/pull/140)
  - An Acton project is a standardized structure for how to organize an Acton
    project:
    - At the root is an `Acton.toml` file. It is currently empty but its
      location denotes the project root.
    - `src/` is the directory in which source files are stored, imports are
      relative to this directory, i.e. `import foo` will import `src/foo.act`
      - imports will also search the system path as before for the stdlib modules
    - `out/` is the output directory in which generated type file (`.ty`) and
      object / library files are written as well as binaries
      - `out/bin` is where executables end up (`actonc --root foo bar.act` will
        produce an executable)
      - `out/types` is where types files go
      - `out/lib` is where object / library files go
  - We used to write compiled output of modules to the system path. To install
    acton on a system, outside of a users home directory, we can no longer write
    to the system path since it is owned by root. Further, we don't want to
    write to a common system path as different users code could collide.
  - Now only the builtins and stdlib modules are in the system path. Other
    compiled output, like type files and object files, are written to the
    project directory.
  - It is still possible to compile individual `.act` files as executabes
    (`--root ROOT`), in which case `actonc` will create a temporary directory
    for intermediate output and write the final file in the same directory as
    the `.act` file. This is to preserve the current behavior when "using acton
    as a scripting language".
  - Overall, the idea is to cater to both:
    - Acton as a scripting language
      - Often a single source file - minimal "project overhead" wanted
    - Acton for large projects
      - Many modules, perhaps producing multiple executables
      - Good project structure necessary to manage it all
- There is now an RTS debug mode
  [#189](https://github.com/actonlang/acton/pull/189)
  - With `actonc --rts-debug`, an Acton program can be compiled with debug
    functionality included.
  - When a debug enabled Acton program is run with `my-program --rts-debug`, 
- Argument to configure the replication factor used with the distributed backend
  database
  [#204](https://github.com/actonlang/acton/pull/204)
  - Default is 3, as that is the lowest value that makes sense for a quorum.
  - Read and write quorum is derived from the replication factor / 2 + 1
    - Thus, with replication=3, read/write quorum is 2
- Connecting to multiple database backend servers for redundancy
  [#206](https://github.com/actonlang/acton/pull/206)
  - `--rts-ddb-host` can be specified multiple times to connect to multiple
    backend servers
  - Thus, an RTS compute node can survive a database server going down.
  - The port number can be specified per host by appending it colon separated
    from the hostname, like `--rts-ddb-host localhost:32001`
- Monotonic time function have been added
  [#152](https://github.com/actonlang/acton/pull/152)
  - `time.monotonic()` returning a float.
  - `time.monotonic_ns()` returning nanoseconds as an integer.

### Changed
- `time` module has been aligned with its Python counterpart
  [#152](https://github.com/actonlang/acton/pull/152)
  [#197](https://github.com/actonlang/acton/pull/197)
  - `time.time()` function now returns a float.
  - New function `time.time_ns()` returns time in nanoseconds as an integer.
  - `RuntimeError` is raised on an error, just like Python
- `actonc --version` now includes verbose output
  [#212](https://github.com/actonlang/acton/pull/212)
  - Previously it was required to run `actonc --version --verbose` to see CC
    information etc. This is now included in the default output of `actonc
    --version` and `--verbose` no longer influences the output.

### Fixed
- `actonc --version` no longer requires a "dummy" argument
  [#212](https://github.com/actonlang/acton/pull/212)
- Fix actor creation and initialization in the RTS
  [#186](https://github.com/actonlang/acton/pull/186)
  - Previously, actor creation and initialization was indeterministic and given
    certain conditions, messages or even actors could be lost, leading to
    system malfunction. One way in which this showed was how certain programs
    would just hang even when they should exit.
  - RTS has been entirely reworked in regards to actor creation and now follows
    the overall language model, which makes things simpler and more robust.


## [0.5.3](https://github.com/actonlang/acton/releases/tag/v0.5.3) (2021-09-13)

### Added
- It is now possible to install Acton on Mac OS X via Homebrew
  - `brew install actonlang/acton/acton`
- Warnings are now generally treated as errors when compiling Acton components
  [#153](https://github.com/actonlang/acton/pull/153)
  - Many warnings have been fixed.
  - There are some exceptions added for some warnings.
    - With time these should be reduced.
- Bug triage and prioritization document
  [#165](https://github.com/actonlang/acton/pull/165)
  - see `docs/triage.md`

### Fixed
- Fix internal instantiations of tuples to use `$NEWTUPLE`
  [#173](https://github.com/actonlang/acton/pull/173)
  - For example functions like `divmod` that returns a tuple was using an
    incorrect macro.
- `divmod` now returns correct results. (#))
  - It would previously return wildly incorrect results (integer wraparound
    style) and could lead to segfaults due to incorrect tuple creation
- Greater than or equal (`>=`) now works for integers
  [#161](https://github.com/actonlang/acton/pull/161)
  - It was broken and actually did an equal match.
- Fix header inclusion for numpy use of arc4random functions
  [#155](https://github.com/actonlang/acton/pull/155)
- Situations where `actonc` was unable to unify atoms and ints or infer type
  conversion have now been fixed.
  [#183](https://github.com/actonlang/acton/pull/183)
- Fix variable being out of scope in a function
  [#174](https://github.com/actonlang/acton/pull/174)


## [0.5.2](https://github.com/actonlang/acton/releases/tag/v0.5.2) (2021-08-25)

### Fixed
- It is now possible to raise exceptions
  [#133](https://github.com/actonlang/acton/pull/133)
  [#137](https://github.com/actonlang/acton/pull/137)
  - Previously a naming misalignment between the Acton compiler code generation
    and the builtins of the RTS led to an Internal Compiler Error.
  - BaseException is now also used instead of Exception, as it should.
- Distributed RTS mode has been fixed with regards to actor bootstrap that
  previously lead to duplicate actors
  [#121](https://github.com/actonlang/acton/pull/121)
- An actor method (callback) can now be passed as an argument to another actor
  method
  [#129](https://github.com/actonlang/acton/pull/129)
  - This could previously lead to a segmentation fault during certain
    situations.
- Avoid cleaning away `modules/math.h`
  [#136](https://github.com/actonlang/acton/pull/136)
  - This was due to an outdated Makefile cleaning target
- Type inferencing now works for all dicts
  [#139](https://github.com/actonlang/acton/pull/139)
  - Previously worked for some, for example a dict of strings but not for a dict
    of ints.
- The modules `acton.rts`, `math` and `random` now work correctly
  [#127](https://github.com/actonlang/acton/pull/127)
  - The type signature filed was missing but is now correctly built.
  - There are test cases for these modules but the tests were not run in CI
- More tests are now run in CI
  [#128](https://github.com/actonlang/acton/pull/128)
  - Due to a directory restructuring and assumptions (mother of all evil), some
    tests were previously not run. While working locally on a developers
    computer things worked due to manually compiled files. We failed to detect
    these missing type signature files in CI since the tests were not run
    automatically.
- Type inference is now order independent
  [#142](https://github.com/actonlang/acton/pull/142)
  - There were previously situations in which the type inferencer was unable to
    do its job based on the layout / order of the code.
- `print(foo())` now correctly prints the return value of foo() rather than a
  message reference (due to asynchronous evaluation)
  [#142](https://github.com/actonlang/acton/pull/142)


## [0.5.1](https://github.com/actonlang/acton/releases/tag/v0.5.1) (2021-08-24)

### Fixed
- v0.5.0 was released with an incorrect version number and would identify itself
  as version v0.4.2


## [0.5.0](https://github.com/actonlang/acton/releases/tag/v0.5.0) (2021-08-23)

### Added
- The distributed database backend is enabled in the RTS
  [#45](https://github.com/actonlang/acton/pull/45)
  - this was previously a build time flag
  - it is now run time configuration via `--rts-ddb-host` and `--rts-ddb-port`
  - see `examples/count` for a demo and instructions
- RTS now has a `--rts-verbose` argument to make it more chatty
  [#45](https://github.com/actonlang/acton/pull/45)
- General evaluation of expressions in a boolean context
  [#107](https://github.com/actonlang/acton/pull/107)
  - explicit boolean expressions were already supported and working
  - however, in Python any expression can be evaluated in a boolean context
    which was not properly supported... and it now is!
  - this could be considered a bug fix, like it should go under "Fixed" but it
    is big enough of a thing to warrant being here... almost like new
    functionality, although we probably did intend to support this all along
- Description of development workflow, see `docs/development_workflow.md`
  [#73](https://github.com/actonlang/acton/pull/73)
- A short guide on how to wrap C libraries for use in Acton, see
  `docs/wrapping_c_libraries.md`
  [#84](https://github.com/actonlang/acton/pull/84)
- `time.time()` function
  [#83](https://github.com/actonlang/acton/pull/83)
  - returns the current system time in nanoseconds as an integer
  - `time` is a new module
- `random.randint()` function
  [#113](https://github.com/actonlang/acton/pull/113)
  - `random` is a new module
- `acton.rts.sleep()` function
  [#95](https://github.com/actonlang/acton/pull/95)
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
  [#77](https://github.com/actonlang/acton/pull/77)
  - use a single top level Makefile rather than recursive Makefiles
  - only matters if you are doing development on Acton itself
  - backend, being enough of a standalone piece, still has its own Makefile
- `actonc --hgen` and `actonc --cgen` now outputs only code
  [#82](https://github.com/actonlang/acton/pull/82)
  - easier to pipe directly to a file
  - compilation command not included, now available with `actonc --ccmd`
- `actonc` argument `--path` is now called `--syspath`
  [#93](https://github.com/actonlang/acton/pull/93)
  - it is the system path and was already referred to as `syspath` internally
- The build output, when building Acton itself, is now placed in `dist`
  [#88](https://github.com/actonlang/acton/pull/88)
  - makes it considerably easier to produce a release by tarring up its content
  - release size has been reduced to almost half by avoiding unnecessary files
  - the top level directories, like `modules` is now a source directory and no
    longer "the working directory of `actonc`"
- Source code of several modules have been moved from separate subdirs, like
  `math` and `time`, into `modules/`. Less top level clutter!
  [#99](https://github.com/actonlang/acton/pull/99)

### Fixed
- Improved dependency declaration for backend
  [#77](https://github.com/actonlang/acton/pull/77)
  - Only matters if you are building Acton yourself and making changes to the
    backend database


## [0.4.2](https://github.com/actonlang/acton/releases/tag/v0.4.2) (2021-08-05)

### Added
- `tip` releases are now available at stable URLs:
  [#70](https://github.com/actonlang/acton/pull/70)
  - https://github.com/actonlang/acton/releases/download/tip/acton-darwin-x86_64.tar.bz2
  - https://github.com/actonlang/acton/releases/download/tip/acton-linux-x86_64.tar.bz2

### Fixed
- Versioned releases now use correct version number, like `0.4.1` rather than
  the version style used for `tip` releases which include the date & time, like
  `0.4.1.2021.08.05.12.15.37`
  [#69](https://github.com/actonlang/acton/pull/69)
- tip release tar balls now reflect the full tip version in the filename
  [#71](https://github.com/actonlang/acton/pull/71)
  - like `acton-linux-x86_64-0.4.1.20210805.13.46.55.tar.bz2`
  - previously, it would just be `acton-linux-x86_64-0.4.1.tar.bz2`, making it
    difficult to differentiate against the proper 0.4.1 or other nightlies


## [0.4.1](https://github.com/actonlang/acton/releases/tag/v0.4.1) (2021-08-05)

### Added
- `--version --verbose` will print verbose version information
  [#41](https://github.com/actonlang/acton/pull/41)
- Internal compiler errors now include information about the C compiler (cc)
  [#41](https://github.com/actonlang/acton/pull/41)
- Acton is now made available as GitHub Releases
  [#53](https://github.com/actonlang/acton/pull/53)
  - pre-built binary releases! Users no longer need to compile Acton themselves
  - available from [Releases page](https://github.com/actonlang/acton/releases)
  - versioned releases, like this v0.4.1, are available
  - latest build from `main` branch is available as `tip`

### Fixed
- Integer subtraction has been fixed
  [#48](https://github.com/actonlang/acton/pull/48)
  - wrong order of operators would previously return wildly incorrect results


## [0.4.0](https://github.com/actonlang/acton/releases/tag/v0.4.0) (2021-07-23)

### Added
- Linux compatibility!
  [#1](https://github.com/actonlang/acton/pull/1)
  [#14](https://github.com/actonlang/acton/pull/14)
  [#15](https://github.com/actonlang/acton/pull/15)
  - it is now possible to compile the Acton compiler (actonc), the backend and
    the RTS on Linux
  - actonc can now compile Acton programs on Linux
  - Linux on x86_64 is tested, no other architectures
  - libkqueue is used on Linux as a compatibility shim between kqueue calls and
    epoll
    - Acton was initially developed on OS X and the RTS therefore uses kqueue
      and not epoll / aio(uring)
- More test cases, in particular the ones for which we have GitHub issues opened
  [#21](https://github.com/actonlang/acton/pull/21)
- CI runs on Linux too
  [#15](https://github.com/actonlang/acton/pull/15)
  - acton is built and tested on both Linux and OS X
  - all tests are the same across both platforms
- `actonc` has got a `--version` argument

### Changed
- Now uses Haskell lts-17.14 instead of lts-13.0
  [#2](https://github.com/actonlang/acton/pull/2)

### Removed
- The project repository has been cleaned up overall

### Fixed
- `actonc` now detects and reports internal compiler errors
  [#28](https://github.com/actonlang/acton/pull/28)
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
