# Changelog

## Unreleased

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
- Remove remaining ending new linse from RTS log messages [#926]

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
[#840]: https://github.com/actonlang/acton/pull/840
[#845]: https://github.com/actonlang/acton/pull/845
[#846]: https://github.com/actonlang/acton/pull/846
[#853]: https://github.com/actonlang/acton/issues/853
[#868]: https://github.com/actonlang/acton/pull/868
[#869]: https://github.com/actonlang/acton/issues/869
[#882]: https://github.com/actonlang/acton/issues/882
[#885]: https://github.com/actonlang/acton/issues/885
[#887]: https://github.com/actonlang/acton/issues/887
[#907]: https://github.com/actonlang/acton/issues/907
[#913]: https://github.com/actonlang/acton/issues/913
[#926]: https://github.com/actonlang/acton/issues/926
[#925]: https://github.com/actonlang/acton/pull/925
[#929]: https://github.com/actonlang/acton/pull/929

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

[homebrew-acton#7]: https://github.com/actonlang/homebrew-acton/pull/7
[homebrew-acton#28]: https://github.com/actonlang/homebrew-acton/pull/28
