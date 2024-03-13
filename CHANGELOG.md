# Changelog

## [0.21.0] (2024-03-13)

## Added
- `acton test` testing has been revamped, now doing multiple test iterations
  - each test will be run for at least 1ms, for many smaller unit tests this
    means the test case run hundreds of times
  - all the internals have been rewritten to handle multiple tests results from
    a test
  - running multiple test iterations gives better performance measurement but
    can also show flakiness in tests
- `acton test list` will list all the test names in a project
- `acton test --name foo --name bar` will filter to only run tests named `foo`
  and `bar` in a project
- AbE: explain testing with `acton test`
- New methods to interact with environment variables [#1723]
  - `getenv` & `getenvb` to read an environment variable
  - `setenv` & `setenvb` to set an environment variable
  - `unsetenv` & `unsetenvb` to set an environment variable
  - The first set of functions work with strings and attempt to automatically
    detect the encoding, although utf-8 is the only supported encoding by Acton
    right now.
  - The functions ending with `b` work with bytes data which can be used for
    explicit control over encoding.
- `env.stdin_install` now supports both `str` and `bytes` [#1723]
  - It was orignally built to work with `str` which meant it decoded data from
    stdin automatically. This assumed utf-8 encoding.
  - `env.stdin_install` was then changed so the callback would get `bytes`,
    which is technically more correct (it could just be bytes!) but leaves it to
    the application programmer to decode data.
  - A new interface now offers the best of both worlds:
    - `env.stdin_install(on_stdin, encoding, on_stdin_bytes)`
    - by specifying `env.stdin_install(on_stdin=my_cb)`, `my_cb` will be called
      and get `str` input. The encoding is detected by reading the `LANG`
      environment variable or defaults to `utf-8`. It is possible to explicitly
      specify the encoding with the `encoding` argument.
    - Use `env.stdin_install(on_stdin_bytes=my_cb)` to instead receive bytes
- Acton now always compiles everything from source
  - Previously, pre-compiled binary libraries were shipped for the native
    target, so on x86_64-linux, there was a libActon.a compiled for x86_64-linux
  - Now we just ship the source code of Acton base etc and perform the full C
    compilation when we actually run actonc
  - The first invokations of `acton build` is slow, taking up to a few minutes,
    but as all results are cached, subsequent invokations run in fractions of a
    second.
  - `~/.cache/acton` is used to store the cache and it is wiped if it reaches
    5GB after which it will be automatically re-populated when the next `acton
    build` is called
- Low level compiler support for dependencies
  - Place Acton project dependencies in the `deps/` folder and the compiler will
    automatically find them to allow inclusion of Acton modules from other Acton
    projects
  - This implements the low level internal support for finding modules in other
    projects and correctly linking together the final output
  - There is NO support for managing the dependency download itself or even
    compiling the dependency
- `SIGABRT` is now automatically handled by the crash handler for automatic
  backtrace printing or interactive debugging, just like we already manage
  `SIGILL` and `SIGSEGV`
  - This simplifies debugging of Acton itself after catastrophic crashes
- `file` module now has methods to interact with file system, like listing
  files, making directories, stating files, removing files and walking directory
  trees.
- `file.FS.exepath()` returns the path to the current executable
- `acton` now finds `actonc` by looking at its own location rather than assuming
  `actonc` is on the path
- Add `--only-act` to only perform Acton compilation and skip C compilation

## Changed
- `acton test` will compile test functions in dev mode to get asserts
- The `dev` and `rel` folders have been removed, e.g. `out/rel/bin` -> `out/bin`
- Printing of compiler pass debug output from individual files, like `acton
  src/foo.act --types` will now only print output from the specified name and
  not dependencies
  - Previously, dependent modules, like if `src/foo.act` imports `bar`, we would
    also print the types debug output for the `bar` module
  - Printing of compiler pass debug output requires recompiling the module and
    thus shortcutting the up-to-date check. By not printing output for included
    modules, the included modules do not need to be recompiled thus speeding up
    the whole compilation.
- Always link in libprotobuf-c to allow modules to use protobuf tech
- Improved argparse parsing by doing a descent first parsing of options and into
  sub-commands with positional argument parsing happening as a second pass
  [#1714]

### Fixed
- `acton new foo` now creates a new project `foo`
- Fixed handling of failing tests in `acton test` [#1709]
- Fixed required since self is now parsed as a kwd param [#1715]
- Upgrade GC
  - Now ignoring `free()`. This was also previously the case but this was lost
    when we upstreamed the new build.zig for bdwgc. It is now back, which is
    required for handling the "edge", like where syscall / libc calls do malloc
    and then free. uv_fs_scandir is an example of such, which was recently added
    and triggered a bug.
  - Incremental collection has been fixed on Linux so that it should work,
    although it is not supported by the Acton RTS. Incremental collection is
    likely much slower for most programs but with better responsiveness. It
    could potentially be better for very large heaps as well as overhead of
    virtual dirty bit now only hits from a very small heap. It remains to be
    seen.
- `testing.test_runner` has been fixed with regards to scheduling, now we
  actually wait for a test kind to be completed before proceeding to the next.
  Previously for async tests we would spawn all tests immediately and then
  proceed to next test category, so we would oversubscribe the tests runners
  giving worse performance.
- Various fixes to test output, like count of tess was wrong
- numpy now compiles and runs. There are new tests cases!

### Testing / CI
- Tests, like of builtins etc, now run with `--dev` so we get assertions


## [0.20.1] (2024-02-22)

### Fixed
- Install `acton` frontend in Debian package

## [0.20.0] (2024-02-22)

### Added
- New `acton` CLI which takes over as the main acton program, it should be
  called instead of `actonc`, for example `acton build` [#1645]
  - it calls into `actonc` for doing most of the work and is currently mostly
    just a thin wrapper
  - over time, more functionality will be placed in the `acton` frontend
- New `acton test` command which build and runs all test in a project [#1645]
- The compiler now does automatic test discovery
  - Simply doing `import testing` and functions with a name starting with
    `_test` will be identified as a test and run by `acton test`. The functions
    are sorted into categories based on their type signature.
- Run-time reconfigurable `malloc`
  - It is now possible to reconfigure which malloc to use during runtime
  - All external libraries (libuv for example) are now configured to use a
    particular malloc
  - Libraries that did not support a custom alloc have now been extended with
    support for custom malloc.
- Module constants are placed in non-GC heap
  - Using the reconfigurable malloc, we now make use of the "real" malloc (the
    one libc provides) during startup of RTS and up past module init. This means
    all module constants will be placed on the normal heap and not the GC heap.
  - The GC is mark & sweep which means it slows down considerably when there is
    a large live heap.
  - This makes is a viable option to place large amounts of data as module
    constants instead of run time state to speed up programs.
- Fixed size integers arithmetic and comparisons are now unboxed
  - For math heavy programs, this can produce a 4x improvement
  - A lot of witnesses are now unnecessary and thus removed, reducing GC pressure
- `testing` module has gotten a face lift with much better asserts
  - The assert exceptions now include and print the values
  - Formatting happens in `__str__` so performance of exceptions is unchanged
- New `snappy` module in stdlib [#1655]
  - Snappy is a fast compression library
  - Snappy is written in C++ which implied some changes to build.zig and actonc
- It is now possible to print a .ty file by doing: `acton foo.ty`
- Improved size & performance of dictionaries
  - An empty dictionary is now completely empty
  - For low number of elements, the dictionary is actually a list with linear
    searching
  - For 5 elements and up, the dict starts to use a hashtable
- `KeyError` & `IndexError` now include key  / index so it's easier to
  understand what went wrong

### Changed
- The Debian package of Acton now depends on libc 2.27 [#1645]
  - Previously depended on the glibc installed on the the build machine (2.36)
    since it was expanded from `${shlibs:Depends}`
- Now using Zig 0.12.0-dev.2236
  - the build system has changed somewhat so all build.zig files are updated
- DB backend is now optional, include with `acton build --db`
- Using newer version of tlsuv library
- RTS main thread now runs special actors, currently just `Env`
  - This enables us to install signal handlers from `Env` since this must be
    done from a programs main thread.

### Fixed
- `dict` changed internally to allow storing of `None`
- Support `None` in JSON
  - Based on the improved support of dicts
- `lstrip`, `rstrip` & `setdefault` now work properly
- Fix effect of `json.encode()` and `json.decode()` [#1654]
  - They are pure!
- Fix `testing.test_runner` when there are 0 tests to run [#]
- Add tests of client & server in `http` module
- `argparse` module now supports nested command parsing
- `argparse` module now supports `--` for literal interpretation of positional
  arguments
- Fix compilation of projects that used TLS
  - We had missed linking in all necessary mbedtls libraries


## [0.19.2] (2024-01-15)

### Added
- `argparse` now supports optional positional arguments
  - like so: `p.add_arg("foo", "Foo thing", required=False, nargs="?")`
- `print()` now takes multiple new input arguments:
  - `print(*vals, sep=" ", end="", stderr=False, flush=False)`
  - `sep` is the separation character between values, default " "
  - `end` is the line ending character, default "\n"
  - Output is written to stdout per default, set `stderr=True` to write to
    stderr instead
  - set `flush=True` to flush the output
- new `--rts-bt-debug` flag that launches interactive debugger (`gdb`) on
  SIGSEGV / SIGILL

### Changed
- `print()` now formats to a temporary buffer before printing to reduce
  interleaving multiple outputs
- `printn()` has been removed in preference of using `print(foo, end="")`

### Fixed
- `KeyError` now includes the key as part of the error message
  - e.g. `KeyError: getitem: key not in dictionary, key: foobar`
  - The string formatting of the error message including the key only happens
    when the str representation of the exception is necessary, so it does not
    incur a performance penalty in normal scenarios
  - The key for which the lookup was attempted is stored in the `key` attribute
- Homebrew Formula now somewhat decoupled from Stack version [#1627]
  - Idiomatic Homebrew Formulas use system-ghc, i.e. a GHC version installed by
    Homebrew itself and not from Stack. Since we specify a Stack LTS resolver,
    which is a coupled to a particular version of GHC, there is room for a
    version mismatch.
  - The latest occurrence of which was GHC 9.4.8 in Homebrew vs Stack LTS 21.13
    with GHC 9.4.7. A rather silly small mismatch but that nonetheless breaks
    the Homebrew build. This is particularly annoying because it happens only
    after we've made a release, so the feedback is too late and thus a
    correction version must be released to fix the version mismatch (we've
    upgraded to LTS 21.25 with GHC 9.4.8 to align on Homebrew).
  - Now `skip-ghc-check` is used to skip the GHC version check to allow some
    minor mismatch to happen. We must still ensure that at least the major
    version of GHC is aligned, like GHC 9.4.
    
    
## [0.19.1] (2024-01-08)

### Fixed
- Upgraded Stack to 21.25 / GHC 9.4.8
  - This should hopefully fix the Homebrew build


## [0.19.0] (2024-01-08)

### Added
- Much improved argument "rows" support [#1609] [#1617], including:
  - positional & keywords arguments
  - default argument values
- `up`, `down`, `left` & `right` in `term` module now accept `n` arg [#1619]
  - can move multiple steps in given direction

### Changed
- for `http.Client` the following positional arguments are now keyword args:
  - `schema`: defaults to `https`
  - `port`: defaults to `80` for schema `http` and `443` for `https`
  - `tls_verify`: default `True`
  - `connect_timeout`: default `10.0`

### Fixed
- Remove superfluous explicit arguments, now using default values [#1615] [#1618]
- Now possible to print `None` [#1609]

### Testing / CI
- Now testing Telemetrify in GitHub Action workflow as part of CI testing
  - Telemetrify is the largest known application written using Acton, so we add
    testing of it to the Acton repo to avoid accidental breakage
  - Telemetrify main branch is already tested with the latest release of Acton
  - now, new changes in acton are also verified against the telemetrify repo
  - we test against the `acton-next` branch in the telemetrify repo
    - this enables us to make breaking changes in the Acton repo, realize the
      break, then write a fix adapting the telemetrify code to the new Acton
      change and push this to the `acton-next` branch after which the PR on the
      Acton repo can be retried


## [0.18.5] (2023-12-12)

### Testing / CI
- Fix build of APT repo


## [0.18.4] (2023-12-12)

### Fixed
- Add constraint forcing type of self parameter [#1598]
- Let solver use upper bound of type variable when reducing protocol constraints
- Remove subclass constraint on isinstance parameters
- Type error message improvements [#1522] [#1589] [#1596]
- Fix `actonc --debug` [#1591]
  - Now also printing zig output
  - `--verbose` has been removed

### Testing / CI
- Stop testing on MacOS 11 [#1600]
  - Homebrew has dropped support for MacOS 11 so the CI job had to build stuff
    from source making it super slow and fragile, thus dropping it.


## [0.18.3] (2023-11-21)

### Added
- `type()` function that provides the type of something as a string [#1587]
  - Can be really useful as development tool to get inferred types or similar
  - It should not be used for metaprogramming or similar
  - This will likely be removed in the future when there is a better way of
    debugging types, like a language server
    
### Fixed
- Only print truly unhandled exception [#1586]
  - "Unhandled" Exceptions are no longer printed in actor methods called by
    another actor that awaits the return value
  - It used to be that when an actor B raised an exception in a method called by
    actor A, presuming no try/except handling at all, an `Unhandled exception`
    message would be printed in both actor A and B.
  - Now we inspect waiting actors and in case there are any, the exception will
    be considered "handled" in the actor method that raised the exception
- Cleanup old cruft from Makefile [#]
- Now using zig v0.12.0-dev.1536
  - Improved CPU feature detection for Apple silicon
  - Some smaller zig build system changes, but it seems to be stabilizing as
    there are now docs!
- Acton now builds on Apple Silicon MacOS [#1582]
  - It used to not understand the CPU features and thus MbedTLS would fail to
    compile
  - With zig v0.12.0, the correct CPU features are recognized
- Fix int rounding [#1577]
- Improve DNS error handling in net.TCPConnection [#1573]
  - Ignore DNS resolution errors if we're already in connected state
  - Given happy eyeball support, we could get a A record and establish a working
    IPv4 connection while IPv6 would give an error and be retried continuously,
    which is completely pointless when IPv4 is already established.
- Type error now separate error from identifier with `:` [#1583]

### Testing / CI
- Now also testing build / test and running on Ubuntu 23.10
- There are now golden tests for the compilers type errors [#1571]


## [0.18.2] (2023-10-31)

A real fix this time to the Debian APT repo.


## [0.18.1] (2023-10-31)

A small fix for Debian builds.

### Fixed
- Align Debian package building on bookworm / 12


## [0.18.0] (2023-10-30)

### Added
- Exceptions now have a `__str__()` method used to print the exception
  - useful for exception types where we want to be able to include dynamic data
  - string formatting of dynamic log messages is expensive, so we don't want to
    incur that cost for exceptions that are thrown but never printed
  - add extra attributes and print them in `__str__()` in your custom exceptions
- Add `testing` module [#1524] [#1552]
  - Supports four categories of tests:
    - unit tests: `proc() -> None`, think pure functions
    - synchronous actor tests, involves actors
    - asynchronous actor tests
    - env tests
- Improvement to type error messages [#1554] [#1556] [#1559]
- Add `term` module
  - a couple of small helpers for doing colored text and similar in terminals
- Add `random.choice()` & `random.choice()` [#1541]
- Add `printn()` is like print but prints without an ending newline
  - also flushes output immediately, useful to print progress style output
  - this is temporary and will be removed once default argument handling is
    properly implemented
- Add `math.pi = 3.141592653589793` [#1539]
- `acton.rts.enable_gc()` & `acton.rts.disable_gc()`
  - there are situations in which it can be useful to turn off the GC, such as
    during certain test scenarios
- `acton.rts.get_mem_usage()` returns memory usage [#]
  - approximate output due to how the GC works
- Add `uri` module [#1560]
  - Features a `parse_http()` function to parse a HTTP URL into its parts

### Changed
- Reverted to global single threaded malloc [#1547]
  - There have been some reports of errors that could potentially be related to
    per-thread malloc or DNS lookups. In caution, the per-thread malloc has been
    disabled and will be brought back in a safe way once we've been able to test
    it much more thoroughly.
- Integer division by zero now results in exception `ZeroDivisionError`
  - for `float`, `inf` is returned [#1530]
    - this is aligned with IEEE & languages like C, Zig, Rust, Swift, Julia
    - Python raises an exception on float divide by zero
- Haskell Stack updated to LTS 21.13 using GHC 9.4 [#1517]
  - Some tests show actonc running ~25% faster
    - Presumably thanks to more optimizations in ghc
  - C++ seemingly pulled in as a dependency for building Acton itself :(
    - Would be nice to use zig c++ but alas, that does not work
- `actonc` now always built statically on Linux
  - Previously it was only built statically in release mode
  - It does emit some errors / warnings around using dlopen but those are
    harmless and can be ignored in our case
- commands in `argparse` now expect `proc()` rather than `pure()`
- JSON encode/decode are now free functions [#1536]
  - the JSON encode and decode functions were implemented as methods on an actor
    due to an earlier actonc bug
  - bug is fixed, so `encode()` and `decode()` are now moved to free functions
    in the `json` module
  - the old `Json` actor remains and simply wraps the free functions

### Fixed
- Fix error handling in `net.TCPConnection` for single family use [#1546]
  - original error handling presumed a dual stack socket and so would wait for
    both IPv4 and IPv6 to report an error before propagating the error to the
    caller
  - now if only a single address family is used, errors are immediately sent to
    the caller
- Fix error handling in `net.TCPConnection` for dual stack use [#1527]
  - while the original design focused on this use case, there was a bug
- Honor `--root`
  - the automatic detection of root actors based on the name `main` would
    previously override a manually provided one
- Fix time difference calculation when underflowing
- Fix add / sub duration & comparison for `time.Instant` [#1545]
- Fix premature conversion of input to close converter [#1553]

### Testing / CI
- Now building .debs on Debian 12 / bookworm
- Avoid DNS lookups in test [#1551]
  - We have a likely bug around DNS lookups that hits on MacOS. Work around by
    using "127.0.0.1" rather than "localhost" in tests.


## [0.17.0] (2023-09-25)

Acton now has exceptions, how exceptional!

### Added
- Add support for exceptions! [#1463]
  - The syntactic support for exceptions has existed for a long time but the
    underlying run time support was not implemented. It now is!
    - It used to be that exceptions would exit the whole application without any
      means of catching an exception.
  - Unhandled exceptions are now printed to stderr [#1481]
    - Do note however that unhandled exceptions will not exit the program nor
      even the actors encountering an exception. It will abort the execution of
      the current method and nothing else.
- New `http` module [#1485]
  - `http.Client` is a HTTP client that supports unencrypted HTTP on port 80 and
    HTTPS using TLS on port 443
  - `http.Listener` / `http.Server` is a HTTP server component that currently
    only supports unencrypted HTTP (due to lack of a `net.TLSListener`)
  - Supports chunked transfer-encoding [#1510]
- New `logging` module [#1483]
  - Provides logging functionality in an actor centric world
- New `argparse` module [#1499]
  - Command line argument parsing!
- `net.TCPConnection`: A new TCP client connection actor [#1398]
  - Support DNS lookups so the input address can be a name rather than an IP
    address
    - It is still possible to connect using an IP address
  - Supports Happy Eyeballs (RFC6555) which uses both IPv4 and IPv6
    simultaneously to connect using the fastest transport
    - Happy Eyeballs is only used for hostnames. If an IP address is provided,
      we connect directly to it.
- `net.TLSConnection`: A new TLS client connection actor [#1470]
  - A simple TLS client connection actor. It supports disabling TLS certificate
    verification for testing purposes.
  - Using the MbedTLS library and tlsuv under the hood.
- `net.is_ipv4(address: str) -> bool` tells you if something is an IPv4 address
- `net.is_ipv6(address: str) -> bool` tells you if something is an IPv6 address
- AbE: Documented capability based security [#1267]
- The class name of exception errors is now printed on exception [#1423]
- `actonc build --cpedantic` now works, it was previously only supported for
  single files [#1438]
- It is now possible to have mutually recursive definitions between function
  defs, classes and actors [#1433] [#1435]
- RTS now uses a per thread malloc implementation [#1456]
  - `malloc` is part of libgc which has two flavours, a global malloc with a
    lock and a per thread allocator
  - During the restructuring to use Zigs build system, the compilation option to
    use the per thread allocator was lost. It is now back!
  - For parallel workloads across multiple RTS worker threads, there is a clear
    bottleneck around malloc... not very surprisingly ;)
- RTS readyQ and per actor message queue has been optimized [#1456]
  - There is now a tail pointer which significantly speeds up insertions
  - The fields have been slightly reorganized to allow the most important fields
    early so it can be fetched in one cacheline
  - Before, having many concurrent actors with many outstanding messages would
    scale poorly, on an AMD 5950X running the pairs benchmark:
    - before:
      -    1 actor pair & 1   token: ~4M continuations per second
      -   10 actor pair & 1   token: ~650K continuations per second
      - 1000 actor pair & 1   token: ~280K continuations per second
      - 1000 actor pair & 500 token: ~280K continuations per second
    - after readyQ tail optimization:
      -    1 actor pair & 1   token: ~3.8M continuations per second
      - 1000 actor pair & 1   token: ~3.6M continuations per second
      - 1000 actor pair & 500 token: ~700K continuations per second
    - after msg queue tail optimization:
      -    1 actor pair & 1   token: ~3.8M continuations per second
      - 1000 actor pair & 1   token: ~3.6M continuations per second
      - 1000 actor pair & 500 token: ~3.6M continuations per second
  - This is an artificial benchmark where there is extremely little real work
    done, so continuation switching becomes the dominant load. Thus, the
    bottleneck becomes the global readyQ. Using more than 1 worker thread only
    leads to lock contention and so the benchmarks are for 1 worker thread.
    - Since the actor message queue is per actor, it is possible that multiple
      worker threads could work faster but in practice they step on each others
      toes enough around the global readyQ that it is slower overall.

### Changed
- `net.TCPIPConnection` is removed and replaced by `net.TCPConnection`
  - Originally opted to add `net.TCPConnection` now and phase out
    `net.TCPIPConnection` later but as there is already a breaking change with
    the change of Auth -> Cap so all user code related to networking (and other
    things) need to be changed, we might as well change from
    `net.TCPIPConnection` to `net.TCPConnection`
- `DNS` actor is replaced with lookup functions [#1406]
  - The `DNS` actor is removed, the `lookup_a` & `lookup_aaaa` methods it has
    are now free functions in the `net` module, i.e. simply call `net.lookup_a`
  - It was originally an actonc shortcoming that lead to the design with a `DNS`
    actor - it was intentional, so this is more of a cleanup
- Renamed authentication concept to "capabilities" [#1402] [#1407]
  - In order to better reflect the mental model and intuition we want to instill
    in users of Acton, Auth objects are renamed to Cap
    - With a reference to a capability, we can use that capability. This aligns
      with the existing security model for the internal actor domain where an
      actor must hold a reference to another actor in order to call its method.
    - We want to extend that mental model for all external things in the world
      as well.
  - All `*Auth` objects now have a `Cap` suffix
  - All `auth` args are renamed to `cap`
- `SysCap` is a new capability for functions related to Acton RTS System
  internals [#1388]
  - `SysCap` is available from `env.syscap`
  - `SysCap` is separate from the normal capability hierarchy where `WorldCap`
    is the root
    - This is to promote the aspiration to restrict access to RTS internal
    - While `WorldCap` shouldn't be passed around frivolously either,
      capabilities should restricted and delegated, it is still fairly normal to
      pass `WorldCap` while the vast majority of programs should not even need
      `SysCap`
- The `msg` of exceptions is now optional [#1422]

### Fixed
- Fix control flow bug for `continue` in `for` loop [#1265]
- Fix control flow bug for `return` in `while loop` [#1194]
- Fix comparison of optional types [#1186] [#1506]
- Fix necessary casts for bool checks on tuple field [#1500] [#1504]
- Throw exceptions for unimplemented builtins [#1010]
  - Previously printed to stderr and did `exit(1)`
  - Now throws a `NotImplemetedError` instead!
- Failed dict key lookups now throw `KeyError` [#1499]
  - Previously incorrectly raised `IndexError`
- Document use of `--sigs` in Acton-by-Example [#1405]
- Fix up-to-date check for stub compilation [#1399]
- Improve support for alternate output in actonc [#1395]
  - When alternate output is enabled, like `actonc --types`, now uses quiet
    mode, thus suppressing various progress messages which interferred with the
    alternate output
  - Avoid errors when compiling files without a root actor
- Clean up old generated output files [#1411]
  - When the source files is moved or removed, also remove the corresponding
    output files
- Fix return value of int hashable [#1415]
- Fix module import check [#1420]
- Avoid segfault when exception `error_message` is not set [#1422]
- Bump Zig version to v0.11.0 [#1421]
- Fix `reversed([])` which would `SIGILL` in dev mode [#1455]
- Fix `reversed([1,2,3])` which now returns reversed result [#1455]
  - Previous code did the reversal but returned the original value :P
- Fix `i64` comparisons like eq/ne/etc [#1459]
  - Would just return the wrong results, like `i64(0) == i64(0)` would not
    return `True`. Now works as they should!
- Fix list add with empty list input, like `[1]+[]` [#1461]
  - Used to segfault, now works as intended

### Testing / CI
- Stop testing on Ubuntu 22.10 since it is End of Support


## [0.16.0] (2023-07-03)

Primarily addition of simplified TCP reconnection, related fixes and functionality.

### Added
- Add `.close()` and `.reconnect()` on TCP client actor [#1383]
  - It is now possible to explicitly close or reconnect a TCP client
- Add RTS Monitor actor in `acton.rts.Monitor()` [#1383]
  - The RTS Monitor actor starts a worker monitor per worker thread, each pinned
    to a particular worker thread. By doing so, it can retrieve information from
    each worker thread in safe manner, honoring the RTS design.
  - Currently just supporting getting handles from I/O subsystem.
- Add `env.nr_threads` [#1383]
- Add `set_actor_affinity(wthread_id)` [#1383]

### Changed
- Include DNS query hostname in `on_error` cb [#1382]
  - Simplifies retrying a query since we otherwise do not have a persistent
    tracking object per query

### Fixed
- Fix `--rts-debug` [#1383]
  - Has been broken since new builder but is now fixed
- Fix off-by-one in worker thread wake up [#1383]
  - Waking all threads would not consider all threads as there was an off-by-one
    error.
  - No known problems in the field from this, it was only noticed in development
    using --rts-wthreads=1
    - The RTS uses at least 4 workers per default
- Use relative source file path in logs [#1383] [#1391]
  - `__FILE__`, set by the C compiler, is used to include source file path in
    log messages
  - Zig uses absolute paths to all files
  - now using `-ffile-prefix-map` to make path relative
  - project dir name also included, for example `base/rts/rts.c`, to make it
    easier to understand from which project a file comes from
- Assignment in multiple branches [#1390] [#1392]
  - Only assigning in multiple branches (not before branches) would lead to
    redefining shadowing variable of outer scope in low level generated C code
  - Info guiding joined assignment has been fixed

### Testing / CI
- Verify we cannot instantiate WorldAuth


## [0.15.3] (2023-06-27)

### Added
- `actonc --cache CACHEDIR` can be used to specify the build cache directory

### Fixed
- static compilation with Musl libc is now possible on Linux [#1372]
  - adjusted to adjtimex
- hashing for `int` now supports values outside of `i64` [#1348] [#1367]
- fix `__pow__` to avoid compiler optimization bug [#1371]
- bump Zig version [#1374]
  - avoids "File not Found" error
- text color in SVG in docs now follow font color [#1365]
- fix hooking of thread creation to inject signal handler wrapper for stop the
  world [#812]
- corrected arguments passed to `on_error` callback of `net.DNS` related lookup
  functions [#812]
  - .act file specified 2 arguments to the `on_error` callback while the .ext.c
    file only sent a single argument

### Testing / CI
- cross-compilation is now tested in CI in all jobs for these targets:
  - `aarch64-macos-none`
  - `x86_64-macos-none`
  - `x86_64-linux-gnu.2.27`
  - `x86_64-linux-musl`
- CI uses `--cache` to place build cache in `~/.cache/acton/test-build-cache`,
  which is in turn cached in CI
  - the cross-compilation testing meant testing went from ~1 minute to ~3, with
    caching it remains at around ~1 minute


## [0.15.2] (2023-06-16)

Elevate Acton's build capabilities by completing the adoption of the Zig build
system. Everything, including external library dependencies, builtins, RTS,
stdlib and backend, is now built using build.zig files. A hierarchy of zig
modules are formed, which allow building the entirety of the Acton system with a
single zig build, which is what actonc calls internally. This enables complete
control over all aspects of the low level compilation.

The most striking feature unlocked is likely cross-compilation:

  $ actonc --quiet helloworld.act 
  $ file helloworld
  helloworld: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.0.0, with debug_info, not stripped
  $ actonc --quiet helloworld.act --target x86_64-macos-none
  $ file helloworld
  helloworld: Mach-O 64-bit x86_64 executable, flags:<NOUNDEFS|DYLDLINK|TWOLEVEL|PIE>

Libraries for the local platform are still prebuilt and included in the Acton
distribution, hich are used when using the default target, i.e. targetting the
local machine. For any customization to the target, everything will be built
from source.

### Fixed
- revamped low level build, now potentially rebuilding entire Acton system from
  source code
  - allows cross-compilation and similar advanced features
  - use `--target` for cross-compilation, e.g. `--target aarch64-macos-none` to
    target an Apple M1 computer
  - for Acton projects, there is now a `build-cache` directory that caches all
    built output and speeds things up tremendously
- `json` module now correctly encodes and decodes floats [#1345] [#1349]
- zig build of all external library dependencies
  - gives us much better control over how libraries are compiled
  - much much faster! autoconfig is really slow!
    - e.g. building protobuf-c takes a few seconds on a 2015 MBP while with
      autoconf, it spends > 1 minute trying to figure out capabilities of the
      C++ compiler, despite the C++ parts of libprotobuf-c not being enabled!
    - the debian test job now often build Acton in less than 3 minutes, macos-13
      often run in ~4 minutes - a real improvement!
  - libargp [#1336]
  - libbsdnt [#1337]
  - libgc [#1344]
  - libnetstring [#1325]
  - libpcre2 [#1331]
  - libprotobuf-c [#1341]
  - libutf8proc [#1332]
  - libuuid [#1340]
  - libuv [#1334]
  - libxml2 [#1333]
  - libyyjson [#1323]
  - remove libbsd & libmd as dependency [#1329]
    - only use was of arc4random in numpy, which is now rand using()
- backend is now built using zig [#1346]
- use curl instead of git clone to fetch dependencies [#1343]
  - much faster and reduced disk usage

### Testing / CI
- caching has been vastly improved in CI
  - based on the new zig build system, we utilize zigs excellent caching
    capability by using a single zig cache for the entire build of Acton. In CI,
    we cache the zig-cache directory, so we don't actually have to recompile
    files at all. Some CI runs now take less than 2 minutes to build Acton!
  - we should be able to  trust Zigs caching to do the right thing, if files are
    modified it will notice!
- testing of the Homebrew Formula has been removed [#1338]
  - since some time, this test job has been intermittently failing, or rather
    only intermittently passing as most of the time it fails
  - to avoid lots of errors in CI, the job has been removed
  - the failures are likely coming from some change in brew behavior
  - we use a fairly hacked up and not supported workflow
  - several attempts to fix it have been unsuccessful and we're unable to spend
    more time on it right not
- removed test of compiling Acton on Ubuntu 20.04 due to problems with stack
  - note how executables built by actonc are still compatible with Ubuntu 20.04


## [0.15.1] (2023-06-02)

A better strategy for constraint solver results in vastly lower constraint
solving times and thus reduced overall compilation times. Large improvements in
code generation resulting in less memory allocations and thus a much lower load
on the GC, yielding much better runtime performance.

The internal compilation of generation C code has been revamped, now more
structured and simplified in many ways, internally making use of the Zig build
system which in turn unleashes a lot of benefits.

### Changed
- release builds now use `-O3` and dev builds use `-Og` [#1270]
- Collection overloading has been removed [#1316]
  - It was previously possible to overload collections like dict / Mapping
  - {} now always means the builtin dict

### Fixed
- Faster constraint solver through new strategy [#1316]
  - Evaluates vastly fewer possibilities
  - One application used to take over 10 mins for constraint solving which now
    runs in roughly 10 seconds - woohoo
- New second stage compilation of generated C code [#1249] [#1304]
  - new `builder` performs "second stage" compilation of the C code that
    `actonc` generates into object files, archives and executable binaries
  - Built on Zig's Build System, gaining all that it offers
  - Very good caching, dependency detection, up-to-date checking and concurrency
  - Custom compilation to make `builder` itself portable
  - Compilation can now be more clearly separated in two steps:
    - step 1: `actonc` compiles `.act` to `.c` & `.h`
    - step 2: `builder` compiles `.c` & `.h` to `.o` & `.a` & executables
  - Up-to-date checking in actonc adjusted to only compare `.act` with `.c` &
    `.h` output
  - `builder` does a fantastic job of following dependencies (looking at
    `#include` statements etc) to figure out when things need to be recompiled
  - All of this is internal, `actonc` calls `builder` under the hood, much like
    it earlier called `cc` under the hood
  - `--no-zigbuild` can be used to force old compilation method
    - `--no-zigbuild` will be removed in the future
- Much faster build of Acton itself using prebuilt libActonDeps [#1279] [#1282]
  - Uses a checksum of git refs of all lib that constitutes libActonDeps
    - If any dep lib is updated to use new version, libActonDeps will be rebuilt
  - Uploaded to github.com/actonlang/libactondeps and downloaded on demand from
    normal Makefile, thus used for local builds and in CI
  - Falls back to local compilation if prebuilt version is not available
- builtins, RTS & stlib now organized as an Acton project called `base` [#1309]
  - More streamlined towards imagined design of how project dependencies will
    work in the future, meaning less special kludges
  - Cleans up include paths and similar, avoiding potential naming conflicts
- builtin code is now generated [#1256] [#1260]
  - `__builtin__.c` and `__builtin__.h` are now generated from `__builtin__.ty`
    at build time, whereas they were earlier checked in to git, having once been
    generated but heavily modified by hand
  - given complexity of these files, it is difficult to keep the files up to
    date when there are changes to the `CodeGen` pass in `actonc`
  - `actonc` CodeGen pass is now infused with some of the bespoke customizations
    done for builtins, other customizations have been removed to streamline the
    whole thing or inserted into .ext.c file to use the "standard way of
    customization" (what an oxymoron)
- Better performance through code generation improvements [#1263] [#1274]
  - Code generation has been improved, primarily to avoid unnecessary
    allocations, thus reducing pressure on GC, leading to improved performance
  - One application that ran in > 12 minutes now run in < 2 minutes
- `xml` module now correctly parses default namespace [#1262]
- address `xml` bugs with more null checks [#126]
- Switched to a nightly version of zig [#1264]
  - pre-release version of zig v0.11
  - we still pin to this particular zig version which goes through the Acton CI
    testing so there is relatively low risk of this silently breaking something
- Document `after` and encourage not using `sleep` [#1269]
- `actonc` now transparently passes through zig output [#1277]
  - treats subprocess output as bytestring instead of trying to decode it
  - we only pass it through anyway, there is no analysis done on it
  - avoids errors seen when not using UTF-8 locale, like when `LANG` is unset
    (effectively defaulting to `C`)
- Remove bunch of old code, irrelevant tests, old gen env scripts ect [#1278]
- `math` module switched from stub style to .ext.c style [#1309]
- Add missing MacOS headers [#1309]
- Remove note on segfault in AbE that's no longer valid [#1303]

### Testing / CI
- Added macos-13 as test target in CI [#1258]
- New CI job to build libactondeps and upload to "cache" repository [#1283] [#1284]
  - Also using cached for CI builds (as well as local)
  - MacOS CI builds 12 -> 6 minutes, Linux ~8 -> 4 minutes
    - brew-macos is still slow, but we get quicker feedback of failures looking
      at test-macos & test-linux jobs
- Fixed Homebrew build by applying workaround
  - Broke due to upstream changes and is still broken with default suggested
    config in `brew new-tap`


## [0.15.0] (2023-04-26)

### Added
- `actonc` is now statically linked in the Acton APT repo packages [#1148]
  - enables development using Acton on Ubuntu 18.04, Debian 10 and distributions
    of similar age
  - static linking only works when the build platform is a slightly older Linux,
    it works on Debian 11 which we are using, while on a newer Debian (testing
    as of December 2022) the compilation fails
  - our static compilation is conditioned on `STATIC_ACTONC=true`, which is set
    in our `build-debs` CI job which builds the .deb packages used in the Acton
    APT repo
  - local compilation still defaults to a dynamically linked `actonc`
- Parallel GC marking, speeding up mark phase significantly on computers with
  many cores [#]
  - Up to 16 parallel threads are used for marking
- Add `sorted()` function [#1211]
  - Uses timsort, which is the same sorting algorithm used by Python and Java's
    Arrays.sort()
- Add `re` module for regular expression matching [#1208]
  - Offers `match()` function to match on text,
    - Unanchored pattern matching, unlike the Python equivalent which is
      anchored at the start per default (seems highly unintuitive)
- Completely revamped `time` module [#1188]
  - The high level interface is now oriented based on the two common usage
    patterns of measuring elapsed time or telling the current date and time.
  - `Stopwatch` measure elapsed time
  - `time.now()` now returns a DateTime object, which is a higher level object
- New fixed width integers: i16, i32, i64, u16, u32, u64 [#1202] [#1202] [#1203]
- Backend queue subscriptions now done in bulk [#1127]
  - An actor mailbox is mapped to a queue in the backend
  - For every actor creation, the RTS would create and subscribe to the
    corresponding queue.
  - There's a new interface for queue subscriptions where individual queues are
    grouped together into queue groups, so the RTS only needs to subscribe to a
    group once
- New acton projects are now git initialized [#1185]
  - Using `actonc new foo` to create a new Acton project, we now check if git is
    available and if so, initialize the repository. Also include a `.gitignore`
- Allow overriding number of worker threads [#1199]
  - It's been possible to set but we always used a minimum of 4, so even setting
    threads to 2 you would end up with 4. It can be really useful for debugging
    to run on a single WT, so honoring this is useful.
- Add list methods `clear`, `extend` & `pop` [#1206]
- New `--rts-no-bt` option to disable automatic backtrace printing [#1226]
  - Automatic backtrace printing is nice for the normal case where one might run
    a program and do not have core dumps enabled
  - However, when doing actual debugging, one usually enables core dumps and
    analysis it using a debugger interactively, in which case it's only annoying
    to get a couple of screenfuls of backtrace in the terminal on every
    iteration
- Laid foundation for exception handling [#1228]
  - Handles exceptions for CPS converted code
  - Proc style functions do not currently handle exceptions properly
  - Not fully enabled just yet, as it requires some more work
- `int` literals between 0 and 256 are now statically allocated [#1235]
  - Python uses the same trick for -5 to 255 for speedup for commonly used
    integers
  - Acton literal negative integers are really not negative integers but an
    expression of "minus" and the integer 5 so we cannot currently do the same
    trick for common negative values, like -1 etc
- lists can now shrink to reduce memory usage [#1237]
  - When list length goes below half, the list will be reallocated and copied
    over to a new memory area

### Changed
- bash completion for `actonc` now completes `.act` files [#1246]
- `TCPIPListener` interface has changed for callback registration [#1181]
  - The `on_receive` and `on_error` handler for individual client connections
    (`TCPListenConnection`) are now registered after instantiation of each
    `TCPListenConnection` actor by calling `cb_install()`.
    - This is most appropriately done in the on_accept callback.
  - Unlike the old pattern, where we provided these callbacks up front to the
    `TCPIPListener` actor, having them registered per client connection
    naturally opens up to having different handlers per client.

### Fixed
- Fix `@property` handling & inference [#1207]
- Fix bug in dict `__eq__`, so equality check now works [#1144]
- `min()` & `max()` now work as they should [#1150]
- `isinstance` now works correctly [#1124]
- Fix compilation with return in nested branches [#1162]
- Signal interrupts are now properly handled in acton.rts.sleep() [#1172]
  - Previously, a signal would interrupt the sleep and it would simply not sleep
    long enough. Now, if interrupted, the sleep is called with the remaining
    time until the full duration has passed.
- Fix worker thread indexes in WTS rtsmon interface [#1176]
- `bool([])` now correctly returns False [#1193]
- Correct `hex`, `bin` and `ascii` functions [#1196]
- `actonc docs --signs` now works [#1197]
- Correct pid is now reported in rtsmon [#1177]
- Atomic mallocs are now used where possible to speed up GC [#1225]
  - The GC does not need to scan the value of a `str` or `bytearray`, so they
    are now atomically allocated which means the GC will skip scanning them
- Document more list operations [#1136]
- Explain named tuples [#1192]
- Improved some parser errors [#1198]
- Now using zig v0.10.1 [#1174]
- Update to LTS 18.28 [#1141]
- Internal name mangling has been changed [#1160]
- builtins has been reorganized [#1169]
- New `make clean-all` target to clean move stuff [#1210]

### Testing / CI
- Add Ubuntu 18.04 as test platform in CI [#1149]
  - It's not possible to build Acton on Ubuntu 18.04
  - As actonc is now statically built, the version built on Debian now also runs
    on Ubuntu 18.04!
- Test all available operations on lists [#1137]
- Test all available operations on dicts [#1140]
- Concurrent builds disabled on MacOS due to intermittent failures [#1145]
- web page rebuild now triggered on changes to docs/acton-by-example [#1216]
- Deb package now also gets a deterministic name [#1217] [#1219] [#1220]
  - Makes it much easier to install the pre-built tip packages for testing / CI
    etc on other repos


## [0.14.2] (2022-11-27)

### Fixed
- Improve `actonc` performance [#1119]
  - The constraint solver can have a massive performance impact on the
    compilation process, in particular if it has a very large amount of
    solutions to evaluate.
  - The number of alternative solutions had too little weight during constraint
    sorting which would result in a very large amount of potential solutions to
    evaluate. An example program took 31 minutes to compile, which after the fix
    compiles in milliseconds.
  - For a 100 constraints, with the wrong strategy we might need to evaluate
    5^100 solutions (heat death of universe etc) whereas if we do things
    correctly we can solve all constraints in perhaps 100*3 tries. Exponential
    is exponential.
- Fixed size `i64` integer type is now instantiable [#1118]
- Improved str to int conversion [#1115]
- Corrected str `.split` and `.splitlines()`
- Correct DB server & client comm loop select handling [#1111]
  - Ignores EBADF and have ensured this design is correct with regards to timing
    of invalid fds.
- Drop explicit `gcc` dependency for Debian package [#1110]
  - Haskell GHC still depends on it though, so it still gets installed.
- Use slightly newer bsdgc / libgc version [#1112]
  - Slight build simplification as we've upstreamed some modifications.


## [0.14.1] (2022-11-14)

### Added
- `actonc --cc` to specify which C compiler to use to compile a module and
  binary executables [#1103]
  - Note that the Acton system is still compiled by `zig cc`. On Linux we are
    targeting GNU libc 2.28, which might affect compilation of individual
    modules or executables.

### Fixed
- `float.__ge__` is now working correctly [#1105]
- Removed incremental operations from integers [#1106]
- Improved `int` to `str` conversion [#1107]


## [0.14.0] (2022-11-10)
Acton RTS now does garbage collection!

### Added
- The Acton RTS now does garbage collection [#1091]
  - Using libgc a.k.a the Boehm-Demers-Weiser garbage collector
  - The GC stops the world to perform garbage collection during which all Acton
    RTS worker threads are paused. It is likely this will be visible as pauses
    of the whole system.
  - Performance appears to be largely on par with and without GC but this is
    based on small artificial programs.
  - It is possible to disable the GC at run time by setting the environment
    variable `export GC_DONT_GC=1` before starting the Acton application
    - This will be removed in the future when the GC has been field proven.
  - Long term is to implement an Acton specific GC that can, for example, avoid
    stop the world events by doing collection per actor. This is quite far into
    the future. Until then, this will have to do!


## [0.13.1] (2022-11-10)

### Changed
- Allow `_` as a dummy variable name [#1020] [#1061]
  - `_` can be used in an assignment to effectively throw away a result
  - Unlike using a variable like `dummy`, `_` acts as a wildcard from a type
    perspective, so that we can do `_ = 3` and `_ = "a"`, while if we attempt to
    use another dummy variable name we will get a type error as we try to assign
    it values with different types
- `__self__` has been renamed to `self` [#1056]
  - it is a reference to the own actor and holds the "external" view, i.e. it
    can be passed to another actor as a reference to the local actor

### Fixed
- Correct DB client comm thread to avoid potential deadlock [#1088]
  - Incorrect handling of error return status from select meant we could attempt
    to read on fds that had no data and thus the comm thread would deadlock.
  - Now we do proper error handling, always continuing for another spin &
    attempt at select in case we get an error back.
  - The (under development) garbage collector (GC) uses signals to instruct
    threads to pause for stop the world events and thus we end up interrupting
    the select loop a lot more frequently than before, thus surfacing this bug.
- Correct DB client comm thread to avoid busy waiting [#1089]
  - On Linux, using `select()` with a timeout, if the select is interrupted the
    timeout value will be modified to reflect the time not slept. Thus the next
    select in our comm thread loop would sleep for a shorter period of time.
    Depending on the timing of the interrupt, the sleep might be shortened to
    effectively form a busy wait.
  - The (under development) garbage collector (GC) uses signals to instruct
    threads to pause for stop the world events and thus we end up interrupting
    the select loop a lot more frequently than before, thus surfacing this bug.
  - Fixed by always resetting the timeout value in the loop.
- Make RTS DB clients stick to one partition [#1087]
  - Make sure RTS (a DB client) does not hop between different DB server
    partitions even if they learn about disjoint gossip views.
  - Prevents incorrect operation when DB servers are not aware of each other but
    are "joined" by a RTS that see all the servers.
  - This scenario is most easily reproduced by starting DB servers without
    specifying a seed, and so the DB servers won't see each other, and then let
    a RTS connect to the DB servers, which then sees a view of all 3 servers.
    - This is now rejected as invalid.
  - Using vector clocks to determine and reject invalid views. Very elegant =)
- Correct `time.monotonic()` [#1097]
  - It returned a wildly incorrect results as the nanoseconds part was not
    properly added up with seconds.
- Include argp-standalone library in libActonDeps [#1058]
  - No more external dependencies!
  - argp is available as part of the system on GNU/Linux but on MacOS we have
    relied on the argp-standalone package installed via brew. We now prefer to
    use our own on both Linux and MacOS.
- Acton system is now compiled with `-Werror` to improve code quality [#1060]
  - There are some exceptions to this but the overall goal is to be essentially
    free of compilation warnings
- Fix bug in truediv for `int` [#1076]
- Fix bad codegen of classname argument to `isinstance()` [#1055]
- Correct effect declaration to `mut` for some builtin protocols [#1053]
- Method decorators like `@staticmethod` now work [#1054]
- Added lost constraints inferred on target expressions [#1050]
- Avoid undefined behavior in builtin object hash [#1065]
- Clean up library include paths etc [#1068] [#1077] [#1080] [#1092]
  - Made possible by including all of our external dependencies in libActonDeps

### Testing / CI
- Add back testing on Ubuntu 20.04 [#1093]
  - libxml2 requires a newer automake (1.16.3) than is available on Ubuntu 20.04
  - We fix this by hacking the configure.ac file to require the version
    available on Ubuntu 20.04 (1.16.1)


## [0.13.0] (2022-11-04)
New "deactorizer", which unlocks proper async / sync actor method calls.

### Added
- Added new "deactorizer" pass in compiler [#374]
  - No real user visible change, like no change in syntax, but we now properly
    compile programs with regards to async / sync calling behavior of methods.
  - Briefly, an actor method called from the local method is called directly.
    This effect is called "proc". Remote actor methods are normally called
    asynchronously and these are called "action". If we assign the return value
    of an action, we are locking for a synchronous behavior which is achieved by
    an await. These semantics are now correctly implemented.
  - In particular, passing methods as arguments, a method might not know whether
    it is passed an action or proc and thus needs to handle this in a generic
    way. This is particularly tricky as we don't want to to any run time
    inspection of arguments and thus need to have it all figured out at compile
    time.
  - Many many many other things are fixed through the merge of the new
    deactorizer. There are improvements and fixes to various passes in the
    compiler. This has been in the works for almost a year.
- Added `--auto-stub` to `actonc` [#1047]
  - Enables automatic detection of stub mode compilation
- Extended actor argument pruning analysis to honour `NotImplemented` [#524]
  - Pruning analysis prunes away arguments that are not used under the lifetime
    of an actor, e.g. an argument only used for actor body initialization code.
    Pruned arguments are not persisted.
  - Pruning analysis does not cover C code, so when one or more methods are
    implemented in C and defined as `NotImplemented` in the Acton module, we
    cannot reliably determine what arguments are used or unused.
  - The safe choice is to assume all arguments are used, which is what we are
    now doing.
  - This removes a bunch of `_force_persistance` methods in stdlib.

### Changed
- Default is now to not automatically detect stub mode [#1047]
  - Use `--auto-stub` to enable automatic stub mode detection

### Fixed
- Now using zig v0.10.0, which was recently released [#1029]
  - Previously using a nightly build of v0.10
- Correct arithmetic operations using hexadecimal literals [#1027]
- Build actondb using zig with -target [#1003]


## [0.12.0] (2022-10-27)
Edvin's second birthday, only 10 minor releases behind Acton ;)

### Added
- Zig is now used as the C compiler for Acton [#972]
  - Many parts of Acton, like the run time system, builtins and parts of the
    standard library are written in C
  - actonc compiles Acton programs into C which are then compiled into binary
    executables using a C compiler
  - zig is now used for both of these use cases
  - zig is bundled with Acton so that we know what version of zig we get, which
    is the same across Linux and MacOS
- Acton programs on Linux are now backwards compatible to GNU libc v2.28
  - Previously, acton programs would be built for the GNU libc version on the
    compiling computer, making it impossible to run on older distributions
  - Now compatible with for example Ubuntu 20.04 LTS & Debian 10
  - This is made possible by zigs incredible cross-compilation functionality,
    which isn't just about targetting other OS, CPUs but also older libc
    versions
  - v2.28 appears to be the oldest version we can practically target without
    code changes, even earlier versions fail compilation
  - We could reduce backwards compatible, for example by targetting glibc 2.31,
    if we find things that we would like access to, i.e. don't regard glibc 2.28
    compatibility as a hard promise
- Added `--always-build` to actonc [#988]
  - Used in test suite so that we always force compilation in order to ensure
    valid test results
- `actonc` argument parsing now done using sub-parsers, allowing greater freedom
  in constructing more complex commands [#976]
- All external library dependencies are combined in one .a archive [#849]
  - Decouples library dependency in builtins, RTS & stdlib from actonc compiler
    arguments
- All external library dependencies are now built from source [#984]
  - Allows us to compile them with zig, thus power to select target libc
- Add `xml` module [#835]
  - Offers simple `encode()` and `decode()` operations
  - Based on libxml2
- The `int` type now supports arbitrary precision [#146]
  - Add some big numbers!
  - The previous implementation of `int` was as a signed 64 bit integer, this is
    now available as the `i64` type in Acton, although its use is currently
    quite limited.
  - Longer term, fixed size integers like `i64` or `u64` (for an unsigned) are
    available and can be chosen for high performance use cases. Developers
    familiar with this level of coding are likely able to make an informed
    choice about what integer type to choose. Using `int` for the arbitrary
    precision type is the safer choice which will make most users happy.
  - Using BSDNT library, which is a reasonably fast C implementation of
    arbitrary precision types and arithmetic operations. It is not as fast as
    MPL but has a more liberal license.

### Fixed
- Fixed seed arg parsing in actondb [#900]
- Avoid crash during TCP client resume
- DB now using logging rather fprintf(stderr, ...)
- Reduce unprovoked CPS & 'rtail' bugs [#949]
- Update docs for --root & `runacton` [#950]
- `file.ReadFile` can now read arbitrarily large files [#955]
- RTS does proper error handling of DB interaction [#957]
  - This is a huge improvement!!!
  - The run time system now properly reads the return code from all DB query
    operations and acts appropriately. For example, lacking a quorum, the RTS
    will continuously retry an operation until quorum can be reached.
  - For some failures a larger chunk of operations needs to be retried, i.e. we
    can't just wrap up a small function in a retry loop but need a larger retry
    loop around a group of operations.
  - DB API now returns a minority status which the RTS can and does react on
    - Typical example is with 3 DB nodes and a commit goes through on 2, i.e.
      with a quorum and is thus considered a success, however the 3rd node
      rejected it because of a schema mismatch. The RTS can now be notified
      through the minority status and attempt to repair the schema.
      - A typical example of schema repairs necessary are for the queues, which
        are dynamic. Every actor gets a queue in the database, so when new
        actors are created, new queues also need to be created and if a
        particular DB server is not up when the queue is created, it will be
        missing and needs to be repaired later on.
      - We should have proper sync up between DB servers, so that they query
        each other and converge on the latest state. Until then, repair
        initiated from RTS is our way of fixing it.
- Notify gossip view to clients (RTS) from agreement round leader [#951]
  - For example, in this scenario:
    - db1 is started
    - RTS is started & connected to db1 but unable to progress due to no quorum
    - db2 is connected, gossips with db1 and we now have quorum
    - RTS continues to be stalled as it is never notified about db2 and thus
      unable to reach a quorum
- Avoid MacOS quarantine shenanigans that kills actonc [#971]
  - MacOS has some quarantine concept, so like if a file is downloaded from the
    Internet, it is marked as quarantine and the user is asked "Are you sure you
    want to run this" after which the quarantine flag, which is stored as a file
    system extended attribute, is cleared.
  - Somehow we trigger this quarantine, presumably by that we overwrite, through
    cp, a binary executable that macos has run and this triggers it to set the
    quarantine flag. The effect is that whenever we execute actonc, MacOS kills
    -9 it, which is obviously rather annoying.
  - By copying to a temporary file and then moving it in place, we avoid MacOS
    setting the quarantine flag. We already did this, though for other reasons,
    for actondb, so in a way this is a unification for the files in bin/

### Testing / CI
- Simplify and clean up RTS DB test orchestrator [#941] [#973]
- Stop testing on Ubuntu 20.04
  - It's not possible to compile libxml2 on a stock Ubuntu 20.04 as a newer
    version of automake is required than is shipped
  - We mainly want to uphold run time compatibility with Ubuntu 20.04, like it
    should be possible to run Acton applications but utilizing Ubuntu 20.04 as a
    development platform for Acton is not a high priority target, thus dropping
    it is quite fine.
- Revamp Increase timeout
- Avoid kill -9 on macos [#969]
  - MacOS quarantine functionality thinks we are doing something fishy and kill
    -9 our process
  - Worked around by doing cp & mv instead of just a cp


## [0.11.7] (2022-10-03)
Many important fixes for RTS/DB and the language in general!

### Added
- Bash completion is now part of the Debian packages & brew formula

### Changed
- actondb now uses a default value for gossip port of RPC port +1 [#913]
  - The gossip protocol only propagates the RPC port & parts of the
    implementation has a hard-coded assumption that the gossip port has a +1
    offset
  - In order to avoid configuration errors, the default gossip port is now RPC
    port + 1 and if another gossip port is explicitly configured, an error log
    message is emitted on startup.
  - While this is marked as a change, it could really be considered a fix as any
    other configuration of the system was invalid anyway.

### Fixed
- Fixed include path for M1 
  - /opt/homebrew/include added to header include path [#892]
  - Actually fixes builds on M1!
  - This has "worked" because the only M2 where Acton was tested also had header
    files in /usr/local/include but on a fresh install it errored out.
- Fix up-to-date check in compiler for imported modules from stdlib [#890]
- Fix seed arg parsing in actondb that lead to "Illegal instruction" error
- Fix nested dicts definitions [#869]
  - Now possible to directly define nested dicts
- Avoid inconsistent view between RTS & DB in certain situations [#788]
  - If an RTS node was stopped & quickly rejoins or if a transient partition
    happens and the gossip round does not complete before the partition heals.
  - We now wait for gossip round to complete.
  - This ensures that local actor placement doesn't fail during such events.
- Fix handling of missed timer events [#907]
  - Circumstances such as suspending the Acton RTS or resuming a system from the
    database could lead to negative timeout, i.e. sleep for less than 0 seconds.
  - The libuv timeout argument is an uint64 and feeding in a negative signed
    integer results in a value like 18446744073709550271, which roughly meant
    sleeping for 584 million years, i.e. effectively blocking the RTS timerQ.
  - It's now fixed by treating negative timeouts as 0, so we immediately wake up
    to handle the event, however late we might be.
- Timer events now wake up WT threads after system resumption [#907]
  - Worker Threads (WT) are created in `NoExist` state and should transition
    into `Idle` once initiated, however that was missing leading to a deadlock.
  - This was masked as in most cases, a WT and will transition into `Working`
    once they've carried out some work and then back into `Idle`
  - `wake_wt` function, which is called to wake up a WT after a timer event is
    triggered, wakes up threads that are currently in `Idle` state, if they are
    in `NoExist`, it will do nothing.
  - If there is no work, such as the case after system resumption from the DB,
    WTs will stay in the `NoExist` state and then `wake_wt` will do nothing, so
    the system is blocked.
  - WT now properly transition into `Idle`.
- Only communicate with live DB nodes from RTS DB client [#910] [#916]
  - When the RTS communicates with the DB nodes, we've broadcast messages to all
    servers we know about. If they are down, they've had their socket fd set to
    0 to signal that the server is down. However, fd=0 is not invalid, it is
    stdin, so we ended up sending data to stdin creating lots of garbage output
    on the terminal.
  - fd -1 is used to signal an invalid fd, which prevents similar mistakes.
  - The DB node status is inspected and messages are only sent to live servers.
- Avoid segfault on resuming TCP listener & TCP listener connection [#922]
  - Invalidate fds on actor resumption [#917]
- Remove remaining ending new lines from RTS log messages [#926]
- Remove ending new lines from DB log messages [#932]

### Testing / CI
- Rewritten RTS / DB tests [#925] [#929]
  - More robust event handling, directly reacting when something happens, for
    example if a DB server segfaults or we see unexpected output we can abort
    the test
  - Now has much better combined output of DB & app output for simple
    correlation during failures
  - Test orchestrator now written in Acton (previously Python), at least async
    IO callback style is better supported to directly react to events...


## [0.11.6] (2022-09-20)

### Fixed
- Homebrew Formula now includes util-linux for Linux
  - required for <uuid/uuid.h>


## [0.11.5] (2022-09-20)

### Fixed
- Homebrew Formula test [#882]
- Homebrew Formula now pins GHC dependency to version 8.10 [#885]
  - This is aligned with the stack resolver version we're currently using
- actondb is now statically linked with protobuf-c [#887]
  - Found in brew linkage testing, which should now pass


## [0.11.4] (2022-09-19)

### Testing / CI
- Minor correction to Homebrew release automation
  - This is silly...


## [0.11.3] (2022-09-19)

### Testing / CI
- Minor correction to Homebrew release automation


## [0.11.2] (2022-09-19)

There are additions but they're so minor you get 'em for free in a patch
release! ;)

### Added
- Add runacton wrapper to run .act files using shebang [#238]
  - Write an Acton program in a .act file and add shebang line at top:
    - `#!/usr/bin/env runacton`
  - ... and then run the .act file. It will be seamlessly compiled and executed!
    Magic *whosh*

### Testing / CI
- Homebrew Formula moved to actonlang/acton repo [#868]
  - Building a brew bottle is now part of regular CI testing
  - Ensures that our Homebrew Formula is correct with regards to dependencies
    etc, which has previously been a long standing issue
  - mirrored to actonlang/homebrew-acton when we release
- Retry DB test jobs [#853]
  - No longer have to manually re-run CI jobs on failures
  - Retry just the DB test itself, which takes a few seconds instead of
    rerunning entire CI job which takes minutes
- New automatic release process
  - Removes adding git tag as manual step
  - Now push branch `release-vX.Y.Z` updating `common.mk` & version in
    `CHANGELOG.md`, once this PR is merged, a new workflow adds the git tag
    which in turn triggers the release build


## [0.11.1] (2022-09-14)

### Testing / CI
- Fix APT repo job


## [0.11.0] (2022-09-14)

### Added
- Apple M1 / M2 CPUs are now supported [#823]
  - aarch64 calling conventions are different for variadic vs fixed functions
    and this is a problem when we alias a fixed function as a variadic function,
    which is exactly what we did in a number of places in the stdlib where we
    manually write C code
  - Now fixed by calling $function2, $function3 etc for 2 vs 3 args
- `actonc new foobar` will create a new project called foobar according to the
  standard project directory layout
- Completely rewritten IO system & RTS workers [#698]
  - libuv is now used for all IO operations instead of our own home grown IO
    subsystem.
  - Rather than a single thread dedicated to IO, all RTS worker threads now also
    perform IO work. IO actors are pinned to a particular worker thread on
    initialization.
- New `net` module [#733]
  - replaces `env.connect()` & `env.listen()`
  - adds DNS lookup functions
- New `process` module [#752] [#796] [#797] [#798] [#799] [#800] [#801] [#804]
  [#808]
  - runs sub-processes
- New `file` module which allows reading and writing files [#806]
  - Replaces `env.openR` & `env.openW`
- New `json` module to encode and decode Acton data to JSON document (strings)
  [#829]
  - built on yyjson C library, which is a very fast (3.5GB/s) JSON library
- Do token based authentication
  - Capability based authentication has previously been based around access to
    the env actor, which was initially fed into the system as an argument to the
    root actor
  - We wanted to modularize the code of the env actor and so instead we've opted
    to use a token based authentication scheme
  - A WorldAuth token is passed to the root actor, accessible as `env.auth`
  - Tokens are organized in a hierarchy, e.g.
    - WorldAuth > NetAuth > TCPAuth > TCPConnectAuth
  - Performing actions always require most specific auth token
    - This is to encourage users NOT to pass around the WorldAuth token but make
      it more specific
  - For example creating `net.TCPIPConnection` requires a `TCPConnectAuth` token
- actonc now outputs some information about what it is doing per default [#840]
  - Previous quieter output can be achieved with `--quiet`
  - `--timing` includes extra timing information on how long compiler passes and
    other operations take
  - all `--verbose` output now under `--debug`
- actonc now takes a lock on a project when compiling, which ensures that two
  invokations of actonc to build the same project do not step on each others
  toes [#760]
  - lock is implemented using flock(), so even if actonc dies and lock file
    stays in place, it is not considered locked (flock only keeps lock while
    process that acquired lock is running)
  - Our own stdlib is built concurrently with development and release profile,
    which can now be run "concurrently", i.e. we do not have to ensure
    serialization of these two builds from the Makefile and since all other
    targets are run concurrenctly this simplifies the Makefile quite a bit,
    however since there is a lock there won't be an actual concurrent build
- actonc now automatically sets the root actor to `main`, if a program contains
  such an actor. This means `--root` argument is mostly superfluous now [#726]
- actonc --root argument now takes a qualified name, like <module>.<actor>
  [#628]
  - this is required to build an executable in a project with:
    `actonc build --root test.main`
- RTS has improved logging, including timestamps, log levels etc. Also supports
  file output [#584]
  - Log output is sent to stderr
  - RTS worker thread id is included so it is now easier to follow what a
    particular thread is doing.
  - stderr output has millisecond time resolution
  - file output (--rts-log-path) has nanosecond precision
  - Using pretty, short relative paths
- ActonDB has improved logging, including timestamps, log levels etc and
  supports file output [#588]
  - supports logging to file with --log-file
  - similar implementation as RTS (based on same log.c library but modified)
- actonc automatically detects stub mode compilation based on presence of .c
  file [#601] [#624]
- actonc supports new .ext.c style definition of C functions where individual
  functions in an .act file can be externally defined in the .ext.c file by
  using `NotImplemented` as the function body [#706]
- actonc is now aware of development / release profile [#599] [#612]
  - output placed in correct location, i.e. out/rel or out/dev
- actonc now supports custom compilation of modules via Makefile [#602]
  - if a module is to be stub compiled (i.e. there is a .act file with a
    corresponding .c file) then if there is also a Makefile in the project root,
    actonc will run make for the corresponding target, e.g. `make out/rel/foo.o`
- actonc now has concept of commands [#608]
  - like `actonc build` which will build an Acton project
- stdlib is now compiled from actonc as a Acton project [#599]
- `__builtin__.act` has been moved from stdlib to `builtin/ty`, which is also an
  Acton project [#623]
  - `__builtin__.act` really is special, like it's an implicit dependency for
    everything else, and trying to add extra logic to get it to build first when
    building the stdlib project doesn't make sense; it's now special rules in
    the Acton Makefile
- Improve DB schema creation messages [#586]
  - Messages contained "test", which is misleading. The DB server creates the
    schemas, i.e. the schema is hard-coded. This is per design and a
    simplification (no need for schema management RPCs) as the DB and RTS are
    tightly coupled anyway.
- ActonDB will now gossip RTS node membership, forming the foundation for doing
  distributed computing
- ActonDB monitor membership information now contains all nodes [#645]
- RTS mon now has a membership view [#645]
- `bytes` type added [#655]
- Library dependencies are now included in the Acton distribution [#698]
- Added `__repr__` method to class value & subclasses [#685]
- Added Ord instances for lists & dicts [#719]

### Changed
- `after` now takes a float argument [#846]
  - Allows sub-second delays
- IO subsystem has been replaced with libuv
- `env.connect()`, `env.listen()` has been replaced with `net` module, i.e.
  `net.TCPIPConnection` and `net.TCPListener`
- file access via `env` has been replaced with the `file` module
- RTS log output is now sent to stderr rather than stdout
- `actonc dump foo.ty` now takes the filename of the `.ty` file to dump as a
  positional argument rather than the old way of using an option (`actonc dump
  --file foo.ty`)

### Fixed
- Avoid segfault in actondb due to uninitialized mon fds [#627] [#633]
- Exceptions during build of executables no longer deletes produced .ty files [#629]
- Fixed DB server & client issues leading to segfaults [#559]
  - Pass skiplist keys by reference
  - Fix duplicate nonces in msg_callbacks skiplist
  - Extend lock protection over msg_callbacks
- DB client no longer asserts & exits on perror [#574]
  - Now handles more errors rather than bailing out
- Properly initialize fd_data entries [#571]
  - Sometimes a slot wasn't properly initialized, which would lead to incorrect
    behavior
- RTS now cleans up and closes fd on EOF [#570]
- RTS fd array is now 1024 slots long [#570]
  - Used to be 100, which means we can address up to 100 fds. Default fd limit
    on most Linuxes is 1024, so we align on that. The proper fix is to have a
    dynamic structure for fd or set it to the same value as the limit (but
    that's bad for large values).
- RTS uses sigaction instead of signal, which should tighten up some edge cases
  [#587]
- Do not print log message on new connection in RTS / env [#607]
- Correct Debian package dependencies [#625]
  - We used the wrong dependencies, shared libraries, when we should have the
    development libraries available, since we are not a "normal" application but
    a compiler, so when we in turn build programs we want to link them as
    statically as possible (sort of, not including libc but most other things)
  - Now installing the .deb file will pull in all the correct dependencies on
    Debian 11 / bullseye and Ubuntu 20.04
- Correct handling of plain and raw bytes / string literals [#655] [#657]
- UTF-8 encoding is now enforced for source files
- All library dependencies are now included in the Acton [#693]
  - For example libuv, libutf8proc, libbsd (on Linux)
  - No longer partially linking some modules like math & numpy
  - We used to partially link in e.g. libbsd into math
    - numpy imports math so importing both math and numpy means we got duplicate
      symbols
    - This was likely a faulty design in the first place as it lead to symbol
      collisions
  - Now all library dependencies are part of the Acton distribution and are
    linked in with the final application at actonc build time
  - Files are stored as e.g. dist/lib/libuv_a.a. The _a suffix is to ensure we
    get the statically compiled library and do not link with a shared .so
    library if one happens to be present on the system.
- C lib guide rewritten for new .ext.c style [#766][#766]
- Custom make file invocations are now more idempotent [#845]
  - Avoid copying .h files which forces make to rebuild targets
    

### Testing / CI
- Tasty (Haskell test library & runner) is now used for testing actonc. Most
  tests have been migrated from test/Makefile to compiler/test.hs [#631]
  - Tasty offers different style testing; unit, golden, property etc, with
    flexible runners that can execute tests in paralell. Has nice, compact and
    colorized output in the terminal and can output to richer formats as well as
    run benchmarking tests.
  - There are now timeouts for all tests run by Tasty
- TCP servers stress test for opening & closing fds in RTS [#570]
- actonc build of projects have some test cases [#623] [#625]
- Now testing on MacOS 12 [#527]
- Stopped testing on MacOS 10 []
- Now testing on Ubuntu 20.04, 22.04 & 22.10 [#700]
- New test jobs for running actonc and its output on a clean machine [#]
  - This installs acton from a .deb file on Linux and from artifact .zip file on
    Mac
  - Ensure that we have all dependencies declared correctly and that we ship all
    necessary files
  - Running on same platforms as normal test, i.e.:
    - MacOS 11 & 12
    - Debian 11
    - Ubuntu 20.04, 22.04 & 22.10


## [0.10.0] (2022-02-25)

### Added
- actors now have a `__resume__` function that is called by the RTS just after
  actors have been deserialized from the database [#517]
  - This is effectively a generic hook point that can be used by certain actor
    implementations to run code when being resumed from the database
- ListenSocket supports resumption [#529]
  - We cannot restore the previous state of a listening socket since that fd and
    all its associated resources in the kernel etc are gone.
  - The next best thing we can do is tell the application, through the error
    callback, that there is an error with the listen socket and it is then up to
    the application to retry, effectively attempting to listen again
- Connection now supports resumption [#546]
  - Similar to ListenSocket, so that upon resumption we invoke the registered
    error handler so that the application can reconnect
  - Unlike ListenSocket, the error callback must first be registered through
    `on_receive` on the `Connection`
  - What's neat is that any application that attempts to handle reconnects will
    also automatically have code for dealing with Acton based actor migration
    and recovery!!!
- `actonc` now takes `--tempdir` option to write intermediate output (generated
  C) to instead of randomly generated directory, which also means the specified
  directory is kept and not deleted on exit [#516]
  - in the generated directory, there is a `build.sh` file that contains the
    same commands that `actonc` would use to produce the final binary output
    from the source files

### Changed
- `on_receipt` is now called `on_receive` on `Connection` [#522]
- `on_connection` is now called `on_connect` on `env.listen` [#535]
- The error callbacks of Connection & ListenSocket now receive the Connection or
  ListenSocket as an argument so that it is easy to identify which instance the
  error pertains too [#547]
- The main program thread is no longer named `main` [#528]
  - The thread name is reported in various tools like top and ps or when there
    is no thread name, the process name is used
  - The process name is more unique than the main thread name (always "main"),
    so we prefer to use the process name

### Fixed
- Initialize `pthread_getspecific` to `NULL` value [#529]
  - Avoids segfault, in particular on MacOS
- `on_connect` callback is now always handed a connection argument [#538]
  - Every since we split out the error handler this was really the case but the
    function signature indicated that it was maybe a Connection and so all
    application code had to be written defensively, checking if the argument was
    None
  - It will never be None and the signature now matches that so application code
    can be written more cleanly
- Fix TCP client connection on Linux with epoll [#542]
  - Ever since we started supporting epoll natively on Linux, rather than the
    libkqueue shim layer, we have had this regression where TCP client
    connections do not work
  - We registered the same fd with different filters in epoll which resulted in
    an error. It's now fixed by first clearing older flags before
    re-registering the fd.
- ActonDB and DDB libraries are now compiled with debug flags [#529]
  - Unlike other parts of RTS that are compiled with debug flags when `actonc
    --dev` is used, we don't have the same possibility of conditional
    compilation here, so meanwhile the DDB things will now always include debug
    flags


### Testing / CI
- Correct DB test so that the return value is actually inspected [#518]
- Generated files now come with newline [#521]
- Work around current issues with serialization of uninitialized values [#518]
- Remove old RTS DDB test [#530]


## [0.9.1] (2022-02-17)

### Fixed
- Fix ActonDB monitor interface initialization [#514]
  - God knows how this ever worked, but it mostly did on Linux. Mac OS X really
    exposed the errors together with the RTS not halting on DDB errors, which we
    should address through [#431]
- Serialize all manually constructed actors [#512]
  - A new convenience function makes it easier to serialize all manually
    constructed actors to the DB and it is used in various places so that all
    currently known actors are serialized
  - The better fix for most of these actors is probably to have CPS converted
    init functions instead but that's some more code to write which is
    cumbersome for manually defined actors


## [0.9.0] (2022-02-15)

### Added
- Add DB client library statistics [#473]
  - We keep count, sum of time and timing buckets per operation type
  - `actonmon` exposes these metrics over the prometheus HTTP endpoint
- RTS mon interface now uses netstrings as line encoding [#494]
  - Much more robust than the previous implementation which relied on luck
  - netstrings are very simple and can be implemented in a few lines of C or
    Python, so we are still considerably simpler than protobuf or similar
- Add mon interface to ActonDB [#499]
  - We only expose membership information
  - Gives us a nice programmatic interface to query the DB for membership info,
    so we can determine whether our DB cluster has converged or not
- Acton RTS now handles `SIGINT` and `SIGTERM` [#509]
  - RTS will gracefully shutdown upon receiving either `SIGINT` or `SIGTERM`
  - If a second `SIGINT` or `SIGTERM` is received while the RTS is gracefully
    shutting down, it will instead exit immediately
- Add `ListenSocket` actor [#506]
  - `env.listen()` now returns a `ListenSocket`, previously it did not return
    anything, which meant that we never got a "handle" on the socket itself so
    we couldn't for example stop listening on a port.
  - Also means we have a more natural place to add logic for what should happen
    on actor resumption.

### Changed
- `env.listen()` now takes a on_error callback function [#504]
  - Previously the on_connect callback would be invoked with a `None` argument
    on error, which meant it wasn't really an "on connect" handler
  - Now we get separate callbacks so each can be implemented much cleaner
- Enough DDB servers must be specified for configured replication factor [#474]
  - With a replication factor of 3, `--rts-ddb-host` must be specified at least
    3 times to point to 3 DB servers
  - A previous short term fix [#427] assumed all DDB servers were running on the
    same host, typically localhost for testing
  - We now let the user figure it out instead, allowing true multi-node operation
  - Long term fix is still to properly implement gossip protocol

### Fixed
- Fix segfault caused by misaligned uuid.h / libuuid on MacOS [#488]
  - used `uuid.h` from system and uuid lib from util-linux package
- Fix actor instantiation in env.c [#481]
  - We failed to create the DB queue on actor instantiation which would lead to
    errors on later enqueues [#472]
  - [#186] changed the way actors are initiated and code generated by actonc
    uses the new correct way whereas the builtin actors like Env, Connection etc
    defined in `env.c` are manually implemented and still used to old way
  - All actors in `env.c` now use the new correct way!
- Fix epoll detection of disconnect [#469]
  - In addition to listening for `EPOLLHUP` we must detect `EPOLLRDHUP`
- Do not rewrite argv for `--rts-ddb-host` option arguments [#476]
- Fix `--opt=arg` style parsing of argument [#479]
  - Argument value was `t=arg` rather than `arg` due to misplaced parentheses
- Do single line logging [#475]
  - Previously had tasks where first a partial log line would be printed, to be
    completed when the task finished. This looks nice in an interactive terminal
    but if anything else logs meanwhile, the output becomes garbled.
- Cleaned up bootstrap removing `ancestor0` [#481]
- Speed up RTS startup when using the database [#493]
  - A `select()` timeout meant we would always pause for 3 seconds
  - We now avoid the timeout entirely by having a "wakeup pipe" that we can prod
    to wake up the comms thread in the RTS
  - For small programs, like most of our tests, we can now startup 3 DB nodes
    and run the program in 30ms or so whereas previously it would take just over
    3 seconds. 100x improvement!

### Testing / CI
- CI jobs now depend on all "previous jobs" which prevents inconsistencies [#478]
  - There are jobs, like the update-apt-repo job, which depend on previously
    jobs, like test-linux.
  - While the test job for Linux could succeed and thus allow the
    update-apt-repo job to proceed, text-macos could fail, meaning the overall
    pipeline status is failed, yet enough has passed to now run the
    update-apt-repo job. Uploading a new version there but not a similar one for
    Mac OS X causes inconsistencies.
- DB test script (`test_db.py`) now uses Python unittest style rather than
  homegrown functions [#501]
  - Makes it easier and more robust to handle setUp & tearDown
- Ensure app is killed during tearDown as to ensure cleanup [#502]
- The test for RTS with DB now has a new test where the test app uses TCP for
  control and exposing information which should make it much faster and more
  robust than relying on stdout parsing.
  - However, it does not currently support testing DB resumption / recovery as
    our TCP listen sockets do not support resumption


## [0.8.0] (2022-01-14)

### Added
- New RTS mon logging option [#464]
  - This new option makes it possible to write the RTS monitor statistics to a
    log file specified with `--rts-mon-log-path`
  - The first stats snapshot is taken just at startup of the application after
    which subsequent stats snapshots are taken at the specified periodicity. A
    snapshot is always taken just before shutdown.
  - The periodicity is specified by `--rts-mon-log-period` in units of seconds
    and the default value is 30
- New RTS mon output option [#464]
  - It is possible to output the RTS monitor statistics just before the exit of
    program by specifying `--rts-mon-on-exit`
  - The RTS monitor statics are printed to stdout.
- Threads now have names, making it easier to debug [#457]
  - "main", "IO", "Mon Socket" and "Worker X" are the currently used names
- Show RTS options and arguments with `--rts-help` [#465]

### Changed
- stdout is now line buffered [#464]
- `--rts-mon` has been renamed to `--rts-mon-socket-path`
  - Since there are now multiple output options for the RTS monitor statistics
    we use a more specific name

### Fixed
- Avoid slow RTS performance when using DDB & the DDB server is too fast [#453]
  - If the DB server responded quickly enough, the client library would enter a
    pthread_cond_timedwait to wait for the response but it had already arrived
    leading to always hitting the timeout [#449]
  - The effect was that the Acton program would run very slowly as each
    continuation would take one DDB communication timeout to process, which is
    set to 10 seconds
  - The DDB client library has been improved to check for the response within
    the mutex lock to avoid this situation
  - RTS in DDB mode is now fast!
- Acton RTS now shuts down "gracefully" [#460]
  - `env.exit()` previously called `exit()` in C, immediately shutting down the
    application & RTS
  - Any currently running continuation (in another worker thread) would be
    abrubtly killed
  - This has been improved so that the RTS will let worker threads complete any
    currently executing continuation and commit that work before exiting the
    application
- Only do CPU affinity when there are as many worker threads as CPU cores [#447]
  - `--rts-wthreads` can be used to specify the number of worker threads and if
    that number does not align with the number of CPU cores in the machine, we
    will not do CPU pinning
  - Previously, the worker threads that overlapped with available CPU cores
    would be pinned whereas the rest would go unpinned.
- Corrected worker thread count message [#446]
  - Previously, the log message (when using `--rts-verbose`) that shows the
    number of worker threads used when there are few available CPU cores would
    show the incorrect number (`0`) [#445]:
    - `**RTS** Detected 2 CPUs: Using 0 worker threads, due to low CPU count. No CPU affinity used.`
  - RTS was actually using more worker threads but the log message was wrong and
    has now been corrected.
- Corrected actual number of worker threads [#457]
  - While we computed the correct number of worker threads to use, when creating
    the threads we completely failed to use that computed value and instead
    unconditionally spawned as many worker threads as there are CPU cores.
  - There was also an off-by-one error on the number of worker threads.
  - This has all been corrected so the correct number of worker threads is now
    created and there is a test case to prove it.


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
[#146]: https://github.com/actonlang/acton/issues/146
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
[#238]: https://github.com/actonlang/acton/issues/238
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
[#315]: https://github.com/actonlang/acton/pull/315
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
[#374]: https://github.com/actonlang/acton/issues/374
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
[#431]: https://github.com/actonlang/acton/issues/431
[#432]: https://github.com/actonlang/acton/issues/432
[#434]: https://github.com/actonlang/acton/pull/434
[#445]: https://github.com/actonlang/acton/issues/445
[#446]: https://github.com/actonlang/acton/pull/446
[#447]: https://github.com/actonlang/acton/pull/447
[#449]: https://github.com/actonlang/acton/issues/449
[#453]: https://github.com/actonlang/acton/pull/453
[#457]: https://github.com/actonlang/acton/pull/457
[#460]: https://github.com/actonlang/acton/pull/460
[#464]: https://github.com/actonlang/acton/pull/464
[#465]: https://github.com/actonlang/acton/pull/465
[#469]: https://github.com/actonlang/acton/pull/469
[#472]: https://github.com/actonlang/acton/issues/472
[#473]: https://github.com/actonlang/acton/pull/473
[#474]: https://github.com/actonlang/acton/pull/474
[#475]: https://github.com/actonlang/acton/pull/475
[#476]: https://github.com/actonlang/acton/pull/476
[#478]: https://github.com/actonlang/acton/pull/478
[#479]: https://github.com/actonlang/acton/pull/479
[#481]: https://github.com/actonlang/acton/pull/481
[#488]: https://github.com/actonlang/acton/pull/488
[#493]: https://github.com/actonlang/acton/pull/493
[#494]: https://github.com/actonlang/acton/pull/494
[#499]: https://github.com/actonlang/acton/pull/499
[#501]: https://github.com/actonlang/acton/pull/501
[#502]: https://github.com/actonlang/acton/pull/502
[#504]: https://github.com/actonlang/acton/pull/504
[#506]: https://github.com/actonlang/acton/pull/506
[#509]: https://github.com/actonlang/acton/pull/509
[#512]: https://github.com/actonlang/acton/pull/512
[#514]: https://github.com/actonlang/acton/pull/514
[#516]: https://github.com/actonlang/acton/pull/516
[#517]: https://github.com/actonlang/acton/pull/517
[#518]: https://github.com/actonlang/acton/pull/518
[#521]: https://github.com/actonlang/acton/pull/521
[#522]: https://github.com/actonlang/acton/pull/522
[#524]: https://github.com/actonlang/acton/issues/524
[#527]: https://github.com/actonlang/acton/issues/527
[#528]: https://github.com/actonlang/acton/pull/528
[#529]: https://github.com/actonlang/acton/pull/529
[#530]: https://github.com/actonlang/acton/pull/530
[#535]: https://github.com/actonlang/acton/pull/535
[#538]: https://github.com/actonlang/acton/pull/538
[#542]: https://github.com/actonlang/acton/pull/542
[#546]: https://github.com/actonlang/acton/pull/546
[#547]: https://github.com/actonlang/acton/issues/547
[#559]: https://github.com/actonlang/acton/pull/559
[#570]: https://github.com/actonlang/acton/pull/570
[#571]: https://github.com/actonlang/acton/pull/571
[#574]: https://github.com/actonlang/acton/pull/574
[#584]: https://github.com/actonlang/acton/pull/584
[#586]: https://github.com/actonlang/acton/pull/586
[#587]: https://github.com/actonlang/acton/pull/587
[#588]: https://github.com/actonlang/acton/pull/588
[#599]: https://github.com/actonlang/acton/pull/599
[#601]: https://github.com/actonlang/acton/pull/601
[#602]: https://github.com/actonlang/acton/pull/602
[#607]: https://github.com/actonlang/acton/pull/607
[#608]: https://github.com/actonlang/acton/pull/608
[#612]: https://github.com/actonlang/acton/pull/612
[#623]: https://github.com/actonlang/acton/pull/623
[#624]: https://github.com/actonlang/acton/pull/624
[#625]: https://github.com/actonlang/acton/pull/625
[#627]: https://github.com/actonlang/acton/issues/627
[#628]: https://github.com/actonlang/acton/pull/628
[#629]: https://github.com/actonlang/acton/pull/629
[#631]: https://github.com/actonlang/acton/pull/631
[#633]: https://github.com/actonlang/acton/pull/633
[#645]: https://github.com/actonlang/acton/pull/645
[#655]: https://github.com/actonlang/acton/pull/655
[#685]: https://github.com/actonlang/acton/pull/685
[#693]: https://github.com/actonlang/acton/pull/693
[#698]: https://github.com/actonlang/acton/pull/698
[#700]: https://github.com/actonlang/acton/pull/700
[#706]: https://github.com/actonlang/acton/pull/706
[#719]: https://github.com/actonlang/acton/pull/719
[#726]: https://github.com/actonlang/acton/pull/726
[#733]: https://github.com/actonlang/acton/pull/733
[#752]: https://github.com/actonlang/acton/pull/752
[#760]: https://github.com/actonlang/acton/pull/760
[#788]: https://github.com/actonlang/acton/issues/788
[#806]: https://github.com/actonlang/acton/pull/806
[#808]: https://github.com/actonlang/acton/pull/808
[#823]: https://github.com/actonlang/acton/pull/823
[#829]: https://github.com/actonlang/acton/pull/829
[#835]: https://github.com/actonlang/acton/issues/835
[#840]: https://github.com/actonlang/acton/pull/840
[#845]: https://github.com/actonlang/acton/pull/845
[#846]: https://github.com/actonlang/acton/pull/846
[#849]: https://github.com/actonlang/acton/issues/849
[#853]: https://github.com/actonlang/acton/issues/853
[#868]: https://github.com/actonlang/acton/pull/868
[#869]: https://github.com/actonlang/acton/issues/869
[#882]: https://github.com/actonlang/acton/issues/882
[#885]: https://github.com/actonlang/acton/issues/885
[#887]: https://github.com/actonlang/acton/issues/887
[#890]: https://github.com/actonlang/acton/issues/890
[#892]: https://github.com/actonlang/acton/pull/892
[#900]: https://github.com/actonlang/acton/pull/900
[#907]: https://github.com/actonlang/acton/issues/907
[#910]: https://github.com/actonlang/acton/pull/910
[#913]: https://github.com/actonlang/acton/issues/913
[#916]: https://github.com/actonlang/acton/pull/916
[#917]: https://github.com/actonlang/acton/pull/917
[#922]: https://github.com/actonlang/acton/pull/922
[#926]: https://github.com/actonlang/acton/issues/926
[#925]: https://github.com/actonlang/acton/pull/925
[#929]: https://github.com/actonlang/acton/pull/929
[#932]: https://github.com/actonlang/acton/pull/932
[#941]: https://github.com/actonlang/acton/issues/941
[#949]: https://github.com/actonlang/acton/pull/949
[#950]: https://github.com/actonlang/acton/pull/950
[#951]: https://github.com/actonlang/acton/issues/951
[#955]: https://github.com/actonlang/acton/pull/955
[#957]: https://github.com/actonlang/acton/pull/957
[#969]: https://github.com/actonlang/acton/issues/969
[#971]: https://github.com/actonlang/acton/pull/971
[#972]: https://github.com/actonlang/acton/issues/972
[#976]: https://github.com/actonlang/acton/pull/976
[#984]: https://github.com/actonlang/acton/pull/984
[#988]: https://github.com/actonlang/acton/pull/988
[#1003]: https://github.com/actonlang/acton/issues/1003
[#1020]: https://github.com/actonlang/acton/pull/1020
[#1027]: https://github.com/actonlang/acton/issues/1027
[#1029]: https://github.com/actonlang/acton/issues/1029
[#1047]: https://github.com/actonlang/acton/issues/1047
[#1050]: https://github.com/actonlang/acton/issues/1050
[#1053]: https://github.com/actonlang/acton/pull/1053
[#1054]: https://github.com/actonlang/acton/pull/1054
[#1055]: https://github.com/actonlang/acton/pull/1055
[#1056]: https://github.com/actonlang/acton/pull/1056
[#1058]: https://github.com/actonlang/acton/pull/1058
[#1060]: https://github.com/actonlang/acton/pull/1060
[#1065]: https://github.com/actonlang/acton/pull/1065
[#1068]: https://github.com/actonlang/acton/pull/1068
[#1076]: https://github.com/actonlang/acton/pull/1076
[#1087]: https://github.com/actonlang/acton/pull/1087
[#1088]: https://github.com/actonlang/acton/pull/1088
[#1089]: https://github.com/actonlang/acton/pull/1089
[#1091]: https://github.com/actonlang/acton/issues/1091
[#1093]: https://github.com/actonlang/acton/pull/1093
[#1097]: https://github.com/actonlang/acton/pull/1097


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
[0.8.0]: https://github.com/actonlang/acton/compare/v0.7.4...v0.8.0
[0.9.0]: https://github.com/actonlang/acton/compare/v0.8.0...v0.9.0
[0.10.0]: https://github.com/actonlang/acton/compare/v0.9.0...v0.10.0
[0.11.0]: https://github.com/actonlang/acton/compare/v0.10.0...v0.11.0
[0.11.1]: https://github.com/actonlang/acton/compare/v0.11.0...v0.11.1
[0.11.2]: https://github.com/actonlang/acton/compare/v0.11.1...v0.11.2
[0.11.3]: https://github.com/actonlang/acton/compare/v0.11.2...v0.11.3
[0.11.4]: https://github.com/actonlang/acton/compare/v0.11.3...v0.11.4
[0.11.5]: https://github.com/actonlang/acton/compare/v0.11.4...v0.11.5
[0.11.6]: https://github.com/actonlang/acton/compare/v0.11.5...v0.11.6
[0.11.7]: https://github.com/actonlang/acton/compare/v0.11.6...v0.11.7
[0.12.0]: https://github.com/actonlang/acton/compare/v0.11.7...v0.12.0
[0.13.0]: https://github.com/actonlang/acton/compare/v0.12.0...v0.13.0
[0.13.1]: https://github.com/actonlang/acton/compare/v0.13.0...v0.13.1

[homebrew-acton#7]: https://github.com/actonlang/homebrew-acton/pull/7
[homebrew-acton#28]: https://github.com/actonlang/homebrew-acton/pull/28
