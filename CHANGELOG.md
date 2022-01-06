# Changelog

## Unreleased

## [0.7.4] (2022-01-06)

### Fixed
- env actor is serialized to database backend during bootstrap [#430]
  - Actor serialization ("snapshotting") to the database usually happens when a
    continuation (roughly an actor method) has been run
  - The env actor might never run, which meant it might not have been
    serialized to the DB, so when resuming an Acton application from the
    database, the env actor could be missing leading to a segfault [#408]
- RTS now connects to all DB nodes to enable replication factor >1 [#427]
  - This is a short term fix [#409]
  - Longer term, the client DB gossip protocol will be enhanced to deal with
    this properly [#432]
  - Tests now using 3 DB nodes
- Fix DB Membership signaling on DB server crash [#429]
- The RTS with DDB test is now using 3 DB nodes and replication factor 3 [#434]
- Avoid some C errors so we compile on Ubuntu [#428]


## [0.7.3] (2022-01-03)

### Fixed
- Fix segfaults in RTS argument parsing [#420]
  - Missing argument to option would previously lead to segfault, which is now
    fixed [#419]
  - Passing option arguments with =, like `--rts-wthreads=8` now works
- Various fixes to C lib wrapping document [#418]
- Renamed minienv to env [#417]
  - Only internal change, does not influence what a user of Acton sees
- Now testing RTS using DDB in CI [#425]


## [0.7.2] (2021-12-15)

### Added
- Homebrew formula revamped to support Apple M1 [homebrew-acton#28]
  - Formula now uses brew installed ("system") ghc instead of stack installed
    GHC, which is more idiomatic Homebrew
  - Mac on M1 users, simply do: `brew install actonlang/acton/acton`
    - Acton will be automatically built from source!
  - No pre-built binary packages ("bottles") as our build environment, which is
    using GitHub Action runners, cannot build those
- `actonc` compiler profiles fully implemented [#403]
  - previously, `--dev` only used to enable RTS debug
  - now, all libraries are compiled twice, for dev and rel, and packaged up into
    libActon_dev and libActon_rel archives
  - dev mode is compiled with debug symbols (`-g`)
  - release mode is using optimized code (`-O3`)
  - final program binary is also compiled with the same flags

### Fixed
- The RTS mode using the distributed backend for storing program state is now
  working [#407]


## [0.7.1] (2021-11-23)

There are currently known regressions:
- using RTS together with the distributed backend database is not working

### Fixed
- `actonc` now explicitly using `-no-pie` on Linux [#390]
  - this aligns with how we already build our object files, shipped in
    libActon.a etc


## [0.7.0] (2021-11-23)

There are currently known regressions:
- using RTS together with the distributed backend database is not working

### Added
- Basic support for Apple M1 silicon [#383]
  - Mostly about keeping track of Homebrew paths, since it uses `/opt/homebrew`
    on Apple silicon vs `/usr/local` on Intel CPUs
  - Not thoroughly tested, but Acton compiles and `actonc` appears to produce
    working programs
- *Acton by Example* book [#370] [#371] [#375]
  - https://www.acton-lang.org/learn/acton-by-example/
  - far from complete, but a decent start
  - using mdBook format
  - using svgbob plugin to draw pretty SVG art using ASCII art
  - source in `docs/acton-by-example`
    - source deliberately kept close (same git repo) to Acton itself, so we can
      easily update docs as we update other things
- The number of worker threads is made configurable [#365]
  - Run an acton program with `--rts-wthreads X`
  - As before, number of worker threads defaults to number of logical CPU cores
    (threads)
- RTS statistics and monitoring functionality [#353]
  - Each worker thread keeps some statistics, like:
    - number of sleeps
    - number of continuations executed
    - time spent executing continuation, counted in bucket less than:
      - 100ns, 1us, 10us, 100us, 1ms, 10ms, 100ms, 1s, 10s, 100s, +Inf
    - bookkeeping overhead, like taking internal locks, enqueueing messages etc,
      also counted in buckets
  - `utils/actonmon` is a simple utility to connect to monitor socket, fetch
    statistics and display them
    - can display in fancy table using `--rich` (using Python rich module)
    - defaults to simple no-frills output that have no extra dependencies
    - `--prom` will expose a http server exposing metrics to be consumed by
      Prometheus in the OpenMetrics exposition format
      - can then be displayed in Grafana or similar
- There are now two compilation "profiles"; development & release [#345]
  - `--dev` implies debug symbols are included and RTS debugging is enabled,
    which was previously enabled with `--rts-debug`
  - release mode is the default and does not include debug symbols nor RTS
    debugging
- `actonc --numeric-version` shows a numeric version number [#346]
  - a stable interface to simply display the actonc version number
  - unlike `--version` which stretches across multiple lines and includes the
    versions of cc, the haskell stack used to build actonc etc
- Debian packages are now added as assets to GitHub releases [#369]
  - .deb have always been available as GH Action job artifact
  - For release versions, the .deb was available via the Acton apt repo
  - Now easily possible to grab historic versions as well as pre-release .deb,
    i.e. from main branch by downloading .deb from GitHub release page

### Changed
- Acton compiler now built using Haskell LTS 18.12 [#335]
- `--rts-debug` option replaced `--dev` [#345]
- GitHub Actions now run for PRs and for main branch and tags [#385]
  - Would previously run both for PRs and for push to a branch, resulting in
    duplicate runs

### Fixed
- Significantly improve performance by reducing RTS worker thread wake ups [#360]
  - Best case scenarios is on the scale of 10x performance with drastically
    reduced syscall overhead
- Fix lib path on Mac OS X on x86_64 [#384]
  - Now includes `/usr/local/lib` in library search path
  - This was previously included through some default mechanism but it is now
    explicit
- Make build more idempotent [#333]
- Fix GNU sed check in Makefile [#344]
- Add `#pragma once` to `numpy.h` [#331]
- Avoid regenerating protobuf generated files [#382]
  - Since the generated files are checked into git, we really do not want to
    regenerate them, thus the Make target has been turned into a NOP
- Fix GitHub Action caching issue by adding version cache key [#339]
- Simplify GitHub issue template for bug report [#367]


## [0.6.4] (2021-09-29)

There are currently known regressions:
- using RTS together with the distributed backend database is not working

### Fixed
- `break` & `continue` now works in `for` loops that are CPS fragmented [#325]
  - Using `break` would previously not work in a `for` loop if the loop was
    broken into multiple continuations during the CPS transform. For example,
    this happens when a call to another actor is made in the `for` loop.
- An installed `actonc` can now find `__builtin__.h` [#327]
  - A previous restructuring of files in order to improve the Makefile and build
    organization led to us having to use weirdly long and relative include
    paths. This ultimately led to run time failures of actonc. Since some of
    these files need to be built both when building actonc itself as well as
    when using actonc, the paths must remain the same in source and release
    output. They did not after [#286], which has now been reverted.


## [0.6.3] (2021-09-28)

There are currently known regressions:
- using RTS together with the distributed backend database is not working

### Added
- Acton now has a apt repository - `apt.acton-lang.io` [#320]
  - Version tagged releases are automatically uploaded.

### Changed
- `actonc` now uses `cc` rather than `gcc` [#303]
- Acton project makefiles now use `$(CC)` rather than hardcoding `cc` [#303]
- Acton is now built and tested on debian:bullseye (11, current stable) [#313]

### Fixed
- Debian packages now depend on non-dev libraries for utf8proc & protobuf [#315]
- Makefile restructuring [#304] [#307]
  - backend and compiler Makefiles have now been folded into the top level
    Makefile.
  - DAG is more complete and more things can run correctly in parallel.
  - stack / ghc build now uses 4 parallel threads, for some speedup (55 seconds
    -> 35 seconds on an AMD 5950X). Higher parallelism did not improve things
    much further.
  - Avoid declaring actonc as a PHONY target, which causes constant rebuilds,
    not it actually has proper dependencies defined so make can understand when
    there is nothing to do.
- Debian packages now reflect tip version number [#307] [#308]
  - Previously debian packages had the "base version", like 0.6.2, even when
    built as a pre-release build. `actonc --version` in the package would still
    correctly identify like e.g. `0.6.2.20210924.16.58.23`
  - Now the .deb file also contains the complete build info, like
    `acton_0.6.2.20210924.16.58.23_amd64.deb`


## [0.6.2] (2021-09-23)

### Added
- Linuxbrew build [homebrew-acton#7]
  - Acton can now be installed via Linuxbrew (Homebrew on Linux)

### Fixed
- Constraint grouping to also consider free environment types variables [#277]
  - Previously, this bug in the constraint solver meant that `actonc` could hang
    when compiling certain programs.
- Added missing CPS case to fix return value [#279]
  - Would previously lead to an internal compiler error where the generated C
    code would not compile.
- Fix RTS bug that could lead to segmentation fault [#285]
  - The integrity of a linked list was not preserved during a certain operation
    which would lead to a segmentation fault.
  - The error typically occurred during high load situations.
- Continuation environment now includes all variables [#288]
  - Previously, after the CPS step, the generated continuation was lacking its
    environment which meant some variables were inaccessible.
- The numpy module is now working again [#293]
  - It was broken during a code restructuring and has now been restored.
  - The entire public interface is now contained in numpy.h (not spread over
    multiple .h files as before).
  - numpy.o is partially linked to hide any library dependencies.
  - The same model of not leaking module internals will be used for other
    modules in the future.


## [0.6.1] (2021-09-19)

### Added
- Debian packaging [#60]
  - There is now a .deb produced and uploaded as part of the artifacts for each
    release on GitHub.

### Changed
- RTS now uses a minimum of 4 worker threads [#263]
  - The default is to use as many worker threads as there are CPUs and set CPU
    affinity to map worker threads to CPUs in a 1:1 fashion.
  - The first core is used to run the eventloop that processes I/O and the rest
    runs the main loop that executes actors.
  - On machines with a low CPU count this can be catastrophic. The extreme
    example being a single CPU machine, which consequently will have 0 worker
    threads executing actors. It will only run the eventloop that processes I/O
    and thus is unable to actually run an Acton program.
  - Even on a 2 CPU machine, which would lead to a single worker thread
    executing actors, this can be quite bad.
  - We now run a minimum of 4 worker threads, 1 for the I/O eventloop and 3
    actor processing threads, to get around these problems.


## [0.6.0] (2021-09-17)

Despite a regression in the RTS's distributed mode of operationa, v0.6.0 is
released as improvements in other areas make it the best version of Acton to
date.

### Added
- Acton project concept [#140]
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
- There is now an RTS debug mode [#189]
  - With `actonc --rts-debug`, an Acton program can be compiled with debug
    functionality included.
  - When a debug enabled Acton program is run with `my-program --rts-debug`,
- Argument to configure the replication factor used with the distributed backend
  database [#204]
  - Default is 3, as that is the lowest value that makes sense for a quorum.
  - Read and write quorum is derived from the replication factor / 2 + 1
    - Thus, with replication=3, read/write quorum is 2
- Connecting to multiple database backend servers for redundancy [#206]
  - `--rts-ddb-host` can be specified multiple times to connect to multiple
    backend servers
  - Thus, an RTS compute node can survive a database server going down.
  - The port number can be specified per host by appending it colon separated
    from the hostname, like `--rts-ddb-host localhost:32001`
- Monotonic time function have been added [#152]
  - `time.monotonic()` returning a float.
  - `time.monotonic_ns()` returning nanoseconds as an integer.

### Changed
- RTS now uses epoll on Linux [#243]
  - Was previously written for kqueue, which runs natively on MacOS X.
  - On Linux, a shim, libkqueue, was used to map kqueue calls to epoll.
  - To reduce risk of error and potentially squeeze out more performance, epoll
    is a better choice.
  - Dependency on libkqueue has been removed, which makes it easier to compile
    Acton on more platforms.
- `time` module has been aligned with its Python counterpart [#152] [#197]
  - `time.time()` function now returns a float.
  - New function `time.time_ns()` returns time in nanoseconds as an integer.
  - `RuntimeError` is raised on an error, just like Python
- `actonc --version` now includes verbose output [#212]
  - Previously it was required to run `actonc --version --verbose` to see CC
    information etc. This is now included in the default output of `actonc
    --version` and `--verbose` no longer influences the output.

### Fixed
- `actonc --version` no longer requires a "dummy" argument [#212]
- Fix actor creation and initialization in the RTS [#186]
  - Previously, actor creation and initialization was indeterministic and given
    certain conditions, messages or even actors could be lost, leading to
    system malfunction. One way in which this showed was how certain programs
    would just hang even when they should exit.
  - RTS has been entirely reworked in regards to actor creation and now follows
    the overall language model, which makes things simpler and more robust.


## [0.5.3] (2021-09-13)

### Added
- It is now possible to install Acton on Mac OS X via Homebrew
  - `brew install actonlang/acton/acton`
- Warnings are now generally treated as errors when compiling Acton [#153]
  - Many warnings have been fixed.
  - There are some exceptions added for some warnings.
    - With time these should be reduced.
- Bug triage and prioritization document [#165]
  - see `docs/triage.md`

### Fixed
- Fix internal instantiations of tuples to use `$NEWTUPLE` [#173]
  - For example functions like `divmod` that returns a tuple was using an
    incorrect macro.
- `divmod` now returns correct results. (#))
  - It would previously return wildly incorrect results (integer wraparound
    style) and could lead to segfaults due to incorrect tuple creation
- Greater than or equal (`>=`) now works for integers [#161]
  - It was broken and actually did an equal match.
- Fix header inclusion for numpy use of arc4random functions [#155]
- Situations where `actonc` was unable to unify atoms and ints or infer type
  conversion have now been fixed. [#183]
- Fix variable being out of scope in a function [#174]


## [0.5.2] (2021-08-25)

### Fixed
- It is now possible to raise exceptions [#133] [#137]
  - Previously a naming misalignment between the Acton compiler code generation
    and the builtins of the RTS led to an Internal Compiler Error.
  - BaseException is now also used instead of Exception, as it should.
- Distributed RTS mode has been fixed with regards to actor bootstrap that
  previously lead to duplicate actors [#121]
- An actor method (callback) can now be passed as an argument to another actor
  method [#129]
  - This could previously lead to a segmentation fault during certain
    situations.
- Avoid cleaning away `modules/math.h` [#136]
  - This was due to an outdated Makefile cleaning target
- Type inferencing now works for all dicts [#139]
  - Previously worked for some, for example a dict of strings but not for a dict
    of ints.
- The modules `acton.rts`, `math` and `random` now work correctly [#127]
  - The type signature filed was missing but is now correctly built.
  - There are test cases for these modules but the tests were not run in CI
- More tests are now run in CI [#128]
  - Due to a directory restructuring and assumptions (mother of all evil), some
    tests were previously not run. While working locally on a developers
    computer things worked due to manually compiled files. We failed to detect
    these missing type signature files in CI since the tests were not run
    automatically.
- Type inference is now order independent [#142]
  - There were previously situations in which the type inferencer was unable to
    do its job based on the layout / order of the code.
- `print(foo())` now correctly prints the return value of foo() rather than a
  message reference (due to asynchronous evaluation) [#142]


## [0.5.1] (2021-08-24)

### Fixed
- v0.5.0 was released with an incorrect version number and would identify itself
  as version v0.4.2


## [0.5.0] (2021-08-23)

### Added
- The distributed database backend is enabled in the RTS [#45]
  - this was previously a build time flag
  - it is now run time configuration via `--rts-ddb-host` and `--rts-ddb-port`
  - see `examples/count` for a demo and instructions
- RTS now has a `--rts-verbose` argument to make it more chatty [#45]
- General evaluation of expressions in a boolean context [#107]
  - explicit boolean expressions were already supported and working
  - however, in Python any expression can be evaluated in a boolean context
    which was not properly supported... and it now is!
  - this could be considered a bug fix, like it should go under "Fixed" but it
    is big enough of a thing to warrant being here... almost like new
    functionality, although we probably did intend to support this all along
- Description of development workflow, see `docs/development_workflow.md` [#73]
- A short guide on how to wrap C libraries for use in Acton, see
  `docs/wrapping_c_libraries.md` [#84]
- `time.time()` function [#83]
  - returns the current system time in nanoseconds as an integer
  - `time` is a new module
- `random.randint()` function [#113]
  - `random` is a new module
- `acton.rts.sleep()` function [#95]
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
- Refactored Makefile structure to complete DAG [#77]
  - use a single top level Makefile rather than recursive Makefiles
  - only matters if you are doing development on Acton itself
  - backend, being enough of a standalone piece, still has its own Makefile
- `actonc --hgen` and `actonc --cgen` now outputs only code [#82]
  - easier to pipe directly to a file
  - compilation command not included, now available with `actonc --ccmd`
- `actonc` argument `--path` is now called `--syspath` [#93]
  - it is the system path and was already referred to as `syspath` internally
- The build output, when building Acton itself, is now placed in `dist` [#88]
  - makes it considerably easier to produce a release by tarring up its content
  - release size has been reduced to almost half by avoiding unnecessary files
  - the top level directories, like `modules` is now a source directory and no
    longer "the working directory of `actonc`"
- Source code of several modules have been moved from separate subdirs, like
  `math` and `time`, into `modules/`. Less top level clutter! [#99]

### Fixed
- Improved dependency declaration for backend [#77]
  - Only matters if you are building Acton yourself and making changes to the
    backend database


## [0.4.2] (2021-08-05)

### Added
- `tip` releases are now available at stable URLs: [#70]
  - https://github.com/actonlang/acton/releases/download/tip/acton-darwin-x86_64.tar.bz2
  - https://github.com/actonlang/acton/releases/download/tip/acton-linux-x86_64.tar.bz2

### Fixed
- Versioned releases now use correct version number, like `0.4.1` rather than
  the version style used for `tip` releases which include the date & time, like
  `0.4.1.2021.08.05.12.15.37` [#69]
- tip release tar balls now reflect the full tip version in the filename [#71]
  - like `acton-linux-x86_64-0.4.1.20210805.13.46.55.tar.bz2`
  - previously, it would just be `acton-linux-x86_64-0.4.1.tar.bz2`, making it
    difficult to differentiate against the proper 0.4.1 or other nightlies


## [0.4.1] (2021-08-05)

### Added
- `--version --verbose` will print verbose version information [#41]
- Internal compiler errors include information about the C compiler (cc) [#41]
- Acton is now made available as GitHub Releases [#53]
  - pre-built binary releases! Users no longer need to compile Acton themselves
  - available from [Releases page](https://github.com/actonlang/acton/releases)
  - versioned releases, like this v0.4.1, are available
  - latest build from `main` branch is available as `tip`

### Fixed
- Integer subtraction has been fixed [#48]
  - wrong order of operators would previously return wildly incorrect results


## [0.4.0] (2021-07-23)

### Added
- Linux compatibility! [#1] [#14] [#15]
  - it is now possible to compile the Acton compiler (actonc), the backend and
    the RTS on Linux
  - actonc can now compile Acton programs on Linux
  - Linux on x86_64 is tested, no other architectures
  - libkqueue is used on Linux as a compatibility shim between kqueue calls and
    epoll
    - Acton was initially developed on OS X and the RTS therefore uses kqueue
      and not epoll / aio(uring)
- More test cases, in particular the ones for which we have GitHub issues [#21]
- CI runs on Linux too [#15]
  - acton is built and tested on both Linux and OS X
  - all tests are the same across both platforms
- `actonc` has got a `--version` argument

### Changed
- Now uses Haskell lts-17.14 instead of lts-13.0 [#2]

### Removed
- The project repository has been cleaned up overall

### Fixed
- `actonc` now detects and reports internal compiler errors [#28]
  - happens when gcc fails to compile the C code that actonc generates
- A bunch of compiler surprises have been made less surprising
  - see git log for details ;)


## [0.3.0] (2021-04-14)

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

[#1]: https://github.com/actonlang/acton/pull/1
[#2]: https://github.com/actonlang/acton/pull/2
[#14]: https://github.com/actonlang/acton/pull/14
[#15]: https://github.com/actonlang/acton/pull/15
[#21]: https://github.com/actonlang/acton/pull/21
[#28]: https://github.com/actonlang/acton/pull/28
[#41]: https://github.com/actonlang/acton/pull/41
[#45]: https://github.com/actonlang/acton/pull/45
[#48]: https://github.com/actonlang/acton/pull/48
[#53]: https://github.com/actonlang/acton/pull/53
[#60]: https://github.com/actonlang/acton/pull/60
[#69]: https://github.com/actonlang/acton/pull/69
[#70]: https://github.com/actonlang/acton/pull/70
[#71]: https://github.com/actonlang/acton/pull/71
[#73]: https://github.com/actonlang/acton/pull/73
[#77]: https://github.com/actonlang/acton/pull/77
[#82]: https://github.com/actonlang/acton/pull/82
[#83]: https://github.com/actonlang/acton/pull/83
[#84]: https://github.com/actonlang/acton/pull/84
[#88]: https://github.com/actonlang/acton/pull/88
[#93]: https://github.com/actonlang/acton/pull/93
[#95]: https://github.com/actonlang/acton/pull/95
[#99]: https://github.com/actonlang/acton/pull/99
[#107]: https://github.com/actonlang/acton/pull/107
[#113]: https://github.com/actonlang/acton/pull/113
[#121]: https://github.com/actonlang/acton/pull/121
[#127]: https://github.com/actonlang/acton/pull/127
[#128]: https://github.com/actonlang/acton/pull/128
[#129]: https://github.com/actonlang/acton/pull/129
[#133]: https://github.com/actonlang/acton/pull/133
[#136]: https://github.com/actonlang/acton/pull/136
[#137]: https://github.com/actonlang/acton/pull/137
[#139]: https://github.com/actonlang/acton/pull/139
[#140]: https://github.com/actonlang/acton/pull/140
[#142]: https://github.com/actonlang/acton/pull/142
[#152]: https://github.com/actonlang/acton/pull/152
[#153]: https://github.com/actonlang/acton/pull/153
[#155]: https://github.com/actonlang/acton/pull/155
[#161]: https://github.com/actonlang/acton/pull/161
[#165]: https://github.com/actonlang/acton/pull/165
[#173]: https://github.com/actonlang/acton/pull/173
[#174]: https://github.com/actonlang/acton/pull/174
[#183]: https://github.com/actonlang/acton/pull/183
[#186]: https://github.com/actonlang/acton/pull/186
[#189]: https://github.com/actonlang/acton/pull/189
[#197]: https://github.com/actonlang/acton/pull/197
[#204]: https://github.com/actonlang/acton/pull/204
[#206]: https://github.com/actonlang/acton/pull/206
[#212]: https://github.com/actonlang/acton/pull/212
[#243]: https://github.com/actonlang/acton/pull/243
[#263]: https://github.com/actonlang/acton/pull/264
[#277]: https://github.com/actonlang/acton/pull/277
[#279]: https://github.com/actonlang/acton/pull/279
[#285]: https://github.com/actonlang/acton/pull/285
[#286]: https://github.com/actonlang/acton/pull/286
[#288]: https://github.com/actonlang/acton/pull/288
[#293]: https://github.com/actonlang/acton/pull/293
[#303]: https://github.com/actonlang/acton/pull/303
[#304]: https://github.com/actonlang/acton/pull/304
[#307]: https://github.com/actonlang/acton/pull/307
[#308]: https://github.com/actonlang/acton/pull/308
[#313]: https://github.com/actonlang/acton/pull/313
[#320]: https://github.com/actonlang/acton/pull/320
[#325]: https://github.com/actonlang/acton/pull/325
[#327]: https://github.com/actonlang/acton/pull/327
[#331]: https://github.com/actonlang/acton/pull/331
[#333]: https://github.com/actonlang/acton/pull/333
[#335]: https://github.com/actonlang/acton/pull/335
[#339]: https://github.com/actonlang/acton/pull/339
[#344]: https://github.com/actonlang/acton/pull/344
[#345]: https://github.com/actonlang/acton/pull/345
[#346]: https://github.com/actonlang/acton/pull/346
[#353]: https://github.com/actonlang/acton/pull/353
[#360]: https://github.com/actonlang/acton/pull/360
[#365]: https://github.com/actonlang/acton/pull/365
[#367]: https://github.com/actonlang/acton/pull/367
[#369]: https://github.com/actonlang/acton/pull/369
[#370]: https://github.com/actonlang/acton/pull/370
[#371]: https://github.com/actonlang/acton/pull/371
[#375]: https://github.com/actonlang/acton/pull/375
[#382]: https://github.com/actonlang/acton/pull/382
[#383]: https://github.com/actonlang/acton/pull/383
[#384]: https://github.com/actonlang/acton/pull/384
[#385]: https://github.com/actonlang/acton/pull/385
[#390]: https://github.com/actonlang/acton/pull/390
[#403]: https://github.com/actonlang/acton/pull/403
[#407]: https://github.com/actonlang/acton/pull/407
[#408]: https://github.com/actonlang/acton/issues/408
[#409]: https://github.com/actonlang/acton/issues/409
[#417]: https://github.com/actonlang/acton/pull/417
[#418]: https://github.com/actonlang/acton/pull/418
[#419]: https://github.com/actonlang/acton/issues/419
[#420]: https://github.com/actonlang/acton/pull/420
[#425]: https://github.com/actonlang/acton/pull/425
[#427]: https://github.com/actonlang/acton/pull/427
[#428]: https://github.com/actonlang/acton/pull/428
[#429]: https://github.com/actonlang/acton/pull/429
[#430]: https://github.com/actonlang/acton/pull/430
[#432]: https://github.com/actonlang/acton/issues/432
[#434]: https://github.com/actonlang/acton/pull/434
[0.3.0]: https://github.com/actonlang/acton/releases/tag/v0.3.0
[0.4.0]: https://github.com/actonlang/acton/compare/v0.3.0...v0.4.0
[0.4.1]: https://github.com/actonlang/acton/compare/v0.4.0...v0.4.1
[0.4.2]: https://github.com/actonlang/acton/compare/v0.4.1...v0.4.2
[0.5.0]: https://github.com/actonlang/acton/compare/v0.4.2...v0.5.0
[0.5.1]: https://github.com/actonlang/acton/compare/v0.5.0...v0.5.1
[0.5.2]: https://github.com/actonlang/acton/compare/v0.5.1...v0.5.2
[0.5.3]: https://github.com/actonlang/acton/compare/v0.5.2...v0.5.3
[0.6.0]: https://github.com/actonlang/acton/compare/v0.5.3...v0.6.0
[0.6.1]: https://github.com/actonlang/acton/compare/v0.6.0...v0.6.1
[0.6.2]: https://github.com/actonlang/acton/compare/v0.6.1...v0.6.2
[0.6.3]: https://github.com/actonlang/acton/compare/v0.6.2...v0.6.3
[0.6.4]: https://github.com/actonlang/acton/compare/v0.6.3...v0.6.4
[0.7.0]: https://github.com/actonlang/acton/compare/v0.6.4...v0.7.0
[0.7.1]: https://github.com/actonlang/acton/compare/v0.7.0...v0.7.1
[0.7.2]: https://github.com/actonlang/acton/compare/v0.7.1...v0.7.2
[0.7.3]: https://github.com/actonlang/acton/compare/v0.7.2...v0.7.3
[0.7.4]: https://github.com/actonlang/acton/compare/v0.7.3...v0.7.4

[homebrew-acton#7]: https://github.com/actonlang/homebrew-acton/pull/7
[homebrew-acton#28]: https://github.com/actonlang/homebrew-acton/pull/28
