# Incremental Compilation

See also [Imports and environments](imports_and_envs.md) for the distinction
between scheduler dependency discovery and the actual import closure loaded into
the type-checker environment.

Acton compiles a whole program by walking a dependency graph that spans the
root project and all of its dependencies. The scheduler in `Acton.Compile`
builds this total graph and runs the front passes (parse, kinds, types, escape
analysis, hashing). A module's parse runs at once. Its later front passes wait
for the front stages of its imports. Back passes are deferred (DBP, deferred
back passes): a module's back job waits until every module of the run that
depends on it, directly or not, has finished its front stage. The exceptions
are listed under [Deferred Back Passes](#deferred-back-passes) below. A module
among them queues its back job as soon as its own front stage finishes. Front
work gates downstream modules, while ready back jobs overlap ongoing front work.
The scheduler runs tasks concurrently with async jobs across worker threads. In
watch/LSP mode, each change event bumps a generation id, cancels in-flight
tasks, and drops stale diagnostics or back jobs that still carry the old
generation, so we can react to new changes without waiting for obsolete work to
finish.

Interface files and content hashes tell the scheduler what work it can safely
reuse. `.tydb` interface headers store hashes and snapshots of dependencies and
their hashes. It is cheap both to compute hashes and to read and compare them.
If the hashes show no relevant change, the compiler can skip parsing, type
checking, or back passes for large parts of the graph.

At the module level we store **moduleSrcBytesHash**, **modulePubHash**, and
**moduleImplHash**. The source-bytes hash (**moduleSrcBytesHash**) is the
SHA-256 of the raw source bytes and is used to decide whether a cached `.tydb`
header is still valid; if it changes, we must re-parse the `.act` file and rerun
front passes for that module. Reading the file is done at GB/s on modern NVMe
hardware and hashing is typically running at roughly the same speed on modern
CPUs.

A module with `NotImplemented` bodies has their C code in a hand-written
`.ext.c` next to its source, which its generated `.c` includes. When a module
has an `.ext.c`, the recorded hash combines the source-bytes hash with the
`.ext.c` hash, so an edit of only the `.ext.c` also invalidates the header. The
`.ext.c` hash is also part of the `implHash` of each `NotImplemented` name, so
the module's generated code and the tests that use those names are redone. A
`.tydb` written while the module had an `.ext.c` records no source stat, so it
is checked by content hash, never by the stat of the `.act` file alone, and a
deleted `.ext.c` is noticed even when the `.act` file is unchanged.

The `.ext.c` hash covers the project files that the `.ext.c` includes, directly
or not (`Acton.ExtSource`). Zig's cache of a whole library cannot stand in for
this: it learns the files a C file includes only when it compiles that file, and
forgets them when a build reuses the file's object, so an edit of only an
included file would reuse the old library. The included files are found by text,
in lines read as the preprocessor reads them (continued lines joined, comments
removed): the operands of every `#include`, `#include_next`, `#import` and
`#embed` directive and every `__has_include`, whatever `#if` encloses them. Each
is resolved as clang resolves it for the include path that `builder/build.zig`
sets up: a quoted name next to the including file, then any name in the project
root, the first `-I`. A file is in the project when the path clang reads it by,
or the file that path leads to, lies in it, so a file reached through a linked
directory counts. A file found elsewhere, in a dependency's or the compiler's
include tree, changes only with them, and a file under `out/` is generated from
the sources, as a module's own header is. When a directive
names its file through a macro, every other file under `src/` counts. Two kinds
of project file stay out of reach: one that only a header outside the project
includes, through the project root on the include path, and, in a scratch build,
one that a directive names through a macro. An `.ext.c` that includes no project file keeps the plain
SHA-256 of its content. For `__builtin__`, whose generated `.c` also includes
`builtin/builtin.c`, that file counts as if its `.ext.c` included it.

A module that embeds files (`embed("path")`) records each of them in the row
`embeds` of its `.tydb`: the path, the size and the SHA-256 of the content, and
the stat of the file, taken before the content was read. The recorded hash
combines the source-bytes hash with the path and the SHA-256 of each embedded
file. Its first front pass, `embedPass`, checks that each path names a file as
`embed` takes it and writes the size and the SHA-256 of the file into the
expression, and the hash of the expression covers them. So the content is part
of the `implHash` of each name that holds the expression and of each name that
uses such a name, in any module, and of no `pubHash`.

An embedded file is read only when its stat does not tell that it is unchanged.
This is the stat rule (`fileFact`): the recorded SHA-256 stands for the content
when the stat of the file is the recorded one and its modification time is
strictly older than that of the `.tydb`. Discovery looks at each recorded file
before it reads the source of the module. It takes the stat, compares each
component of the path with a listing of its directory, which a plan lists once,
and hashes the file only when the stat rule does not hold. The header is
trusted without a read of the source when the source and every recorded file
are found as they are recorded, and else when the recorded hash equals the hash
of the source and of the embedded files as they are now. The recorded stats are
then refreshed in place (`updateEmbeds`), so a file that was written again with
the same content is hashed once and costs one stat in the next build. New
content makes the module a `ParseTask`, and so does a recorded file that is
gone, is no regular file, cannot be read, has 2^31 bytes or more or is spelled
otherwise: `embedPass` then reports the file at its expression.

The compile context holds two maps of what its plan has seen (`ccFiles`): the
stat and the SHA-256 of each file that it looked at, and the names of each
directory that it listed. A later look of the same plan takes the SHA-256 and
the names from there. So a plan reads a file at most once, also when several
modules embed it, lists a directory once, and `embedPass` reads no file that
discovery looked at. A look that finds another stat than the first look of the
plan means that the file changed while the build ran: the module fails with
that message at the expression. `acton test` and the documentation index read
module headers outside a plan, each with maps of its own.

`embedPass` takes the recorded stats from the row of the module's `.tydb` as it
is when the pass runs. The language server keeps a discovered module over many
compiles, and each run of the module's front passes writes the row again. So a
module of a plan that it kept reads no embedded file that passes the stat rule.
A file that does not pass it is read, and front passes that complete write the
stat of that look to the row.

The public hash (**modulePubHash**) is derived from the module's public-name
`pubHash` values (which already include any external signature dependencies).
The implementation hash (**moduleImplHash**) is derived from per-name
`implHash` values; it is stored in the `.tydb` header.

Generated `.c` and `.closures.inc` files are tagged with a codegen tag, on a
first line that reads "Acton impl hash" for historical reasons. It covers the
module implementation hash and what else the back passes read:

- the module's imports, in `.tydb` header order: the generated code calls the
  init function of each and includes its header;
- the selected top-level names of a DBP module;
- where the code names it, the source path relative to the project: the
  `.ext.c` that a module with `NotImplemented` bodies includes and the file of
  each `#line` directive (it differs from the module name only in a scratch
  build);
- unless `--dbg-no-lines` is given, the source hash, since an edit that only
  moves lines moves the `#line` directives.

All of these are in the `.tydb` header, so a cached module's expected tag is
computed without reading its statements (`codegenTag`).

The `.h` declares only the names of the module's source, which the modules
that import it use. The names that the compiler makes up, such as the
closures, functions and continuations that lambda lifting moves to the top
level and the witnesses of top-level expressions, are declared at the top of
the module's own `.c` instead, since no other module uses them. The `.h` is
tagged with the hash of its own text (`headerTag`), which the `.c` records
after its own tag and stamp, and it is written only when its text changes. So
an edit of a body or of comments, or of the module's `.ext.c` or a file that
it includes, leaves the `.h` as it is, and Zig reuses the object of every
module that includes it and whose own code is unchanged. A module that uses a
name whose implementation changed gets a new impl hash, and with it a new
tag and a new object, although its code is the same. The `.h` of a DBP module
declares its selected names, so a body that starts to use a name of its module
that nothing selected before adds that name to it.

If the tag of the `.c` or the `.closures.inc` is missing or mismatched, the
`.h` carries another tag than the `.c` records, another build of the compiler
wrote a file, or the `.c` or the `.closures.inc` is older than the compiler
binary, we rerun back passes even if no other hashes changed. The back passes
write the `.h` before the `.c`, so a `.c` with the expected tag names the `.h`
generated with it, and an `.h` that a later, interrupted job wrote carries
another tag. The `.h` needs no age check, since its tag is the hash of its
text. A `.c` that records no `.h`, as a missing `.c` or one that another build
of the compiler wrote does not, decides alone, and its `.h` only has to carry a
tag. The front passes write the `.tydb` before the back passes write the code, so this
check also catches the code that such a build leaves behind: `acton build
--sigs` and `acton sig` write the `.tydb` but not the code, and so does a build
that is interrupted or cancelled in between. `__builtin__`, which only the base
library compiles, and before every other module (`compileBuiltin`), makes the
same check and runs its front passes again when its code is out of date.

Every `.tydb`, generated file and documentation page records the build of the
compiler that wrote it: the `.tydb` in its `compiler` key, a file as a stamp
after its tag on its first line, the first 16 hex digits of a hash of the
build's identity. A build trusts only the outputs of its own build, and only
those that are not older than its binary, a generated `.h` excepted
(`trustsCompiler`, `compilerNewerThan`); `--ignore-compiler-version` turns both
checks off.
Another build may write other outputs from the same inputs, which no hash shows,
and the age of the binary alone does not show it: a release, a Homebrew install,
a build the user switches back to, or a dependency in the shared
`~/.cache/acton/deps` that another installation built, can be older than the
outputs of a newer build. The identity is `GlobalOptions.compilerIdentity`,
which `acton` and `lsp-server-acton` set to their package version. `make`
gives both the same version, with the build time for a development build
(`VERSION_INFO`), so the two programs of one build share it, and with it the
`out/` directory of a project, while every build has its own. A binary rebuilt
by `stack` alone keeps its version, so the age check stays: such a binary is
newer than the outputs of the build before it. Programs that use the library
directly, such as the test suites, leave the identity empty.

A module that embeds files has a fourth generated file, the data C file
`<module>.embed.c`. It holds an array for each embedded file with content,
which the C compiler fills from the file itself (`#embed`). The generated `.c`
declares the arrays, and each `embed` expression is a `bytes` value that
borrows its array. The path of each `#embed` is written from the real directory
of the data C file, where the C compiler looks first: with `../` once per level
up to the directory that the paths count from, or with the absolute path of
that directory when the data C file does not lie below it. That is so for a
file that is compiled without a project and for a project whose `out` is a
symbolic link. The data C file has a tag of its own (`dataTag`): a hash of the
module name, of that prefix and of the path, size and SHA-256 of each recorded
file. It does not cover the implementation hash. Its first line holds the tag
and the stamp of the compiler, as the first line of a `.closures.inc` does. A
back job writes the file only when it is missing, carries another tag, was
written by another build of the compiler or is older than the compiler binary,
and removes it when the module embeds no file. So an edit of other code of the
module leaves the file as it is, and the C compiler does not read the embedded
files again. New content of an embedded file changes the tag of the `.c` and of
the data C file and leaves the `.h` of the module as it is. New text of the
`.h` of a module of a dependency makes Zig compile every C file of each project
that depends on it again, the data C files among them, since the directory of
the headers of a dependency is an include directory of each of those C files. A
data C file that a back job would write reruns the back passes, as a `.c` or a
`.closures.inc` that is out of date does. `builder/build.zig` compiles a source
whose name ends with `.embed.c` with `-fno-lto`, so that a build with link-time
optimization gets the content as an object file and not as bitcode.

The C compiler reads an embedded file when zig compiles the data C file, which
is after the plan looked at the file. The `_Static_assert` of the data C file
stops the C compile for a file of another size and for a file that is gone, and
the command line compiler reports a failed zig as an internal compiler error.
Nothing stops the C compile for other content of the same size. So the command
line compiler looks at the embedded files once more after zig, also when zig
failed (`changedEmbeds`). It takes each file that the `.tydb` of a module of
the plan records, for the modules that have a data C file. The stat of the file
must be the one that the plan saw, else the build fails with `<path> changed
while the build ran. Run the build again`. And the SHA-256 that the plan has
for the file must be the recorded one, else the build fails with `<path>
changed since its module was compiled. Run the build without --only-build`: a
build with `--only-build` runs no front pass, so it leaves the row as it was
when its plan found other content. A file that the plan looked at is not read
again. So the look costs one stat per module, and for a module that embeds
files one read of its header and one stat per file. It compares with the stat
that the plan saw and not with the recorded one, so for a recorded stat that
could not be refreshed the look reads nothing and reports nothing. A message of
this look is printed in place of the report of a failed zig. The language
server runs no zig and does not look.

An absolute prefix is written into each `#embed` line as it is, so `embedPass`
refuses one that has a double quote or a control character. The data C file of
a module `tool` has the path of the generated `.c` of the module of a source
file `tool.embed.act`. Discovery stops with a project error when two source
files of a project have modules with a generated file in common
(`Project.enumerateProjectModules`). A source file `tool.embed.act` with no
`tool.act` beside it is compiled as any other module. The name of its generated
`.c` ends with `.embed.c`, so `builder/build.zig` compiles the code of that
module with `-fno-lto` too.

Builds of several projects can share a dependency: its outputs live in its own
directory, and those of a fetched dependency in the shared `~/.cache/acton/deps`,
which every project that uses that version shares. So a build takes the work
lock of every dependency project besides its root project's (`.acton.lock`),
from the moment its plan knows them, before it prunes or reads any of their
outputs, until its final build, which compiles their code, is done
(`lockDependencies`). Without the locks, a build of another compiler could
rewrite a dependency's `.tydb` in its own format after this build had checked
it, and the build would then fail on it, or compile C of two compilers
together. With them, the second build waits ("Waiting for compiler lock in
...") and then finds the other format a cache miss, as any other mismatch,
and compiles the dependency again. A build takes every free lock and, when one
is busy, gives back the ones it just took before it waits, so two builds never
wait for each other. The distribution's projects are not locked, since no
build writes them, and neither is a dependency in a read-only directory. A
`.tydb` that still turns invalid under a build, as one written by a compiler
that does not take these locks, fails the module that reads it with a
diagnostic that says so, instead of stopping the build, and the next build
compiles it again.

Hashing within a module is done per top-level name. A hashable unit is a `Def`,
`Actor`, `Class`, `Protocol`, `Extension`, or any top-level value introduced by
a binding statement such as `a = 123` or `b, c = f()`. Entries are keyed by the
local `Name`; external references are stored as `QName` (canonicalized to
`GName` with `unalias`), and extensions use the derived name from
`extensionName`. For now class/actor implementation hashes are coarse and cover
the entire body; per-method hashing can refine this later.

Each unit has three hashes. **srcHash** is the hash of the kinds-checked AST for
that name (pretty-printed), and is the earliest signal that a single declaration
changed. We use the kinds-checked form because it is the first pass that
normalizes type-level syntax (implicit type arguments, canonical kind structure)
without paying the cost of full type inference. Hashing the raw parsed AST would
make equivalent signatures hash differently and trigger needless rebuilds.
**pubHash** is the hash of the unit's public interface (doc-free `NameInfo`)
combined with the public hashes of external declarations referenced by its type
signature. **implHash** is the hash of the unit's implementation combined with
the implementation hashes of external declarations referenced from the body.
Because we hash pretty-printed source, docstrings currently influence
`implHash`; we can tighten the normalization later without changing the overall
model.

Name hashes depend on `NameInfo`. The hash pass takes the `NameInfo` of the
module's names from the interface that `Acton.Types.reconstruct` returns, the
one stored in `.tydb`.

To detect changes without recompiling, each per-name entry in `.tydb` stores
dependency snapshots. **pubDeps** is a list of `(QName, pubHash)` for external
declarations referenced from the unit's type signature and from value-level
uses, which is what triggers re-typechecking when a provider's public interface
changes. **implDeps** is a list of `(QName, implHash)` referenced from the body
and is what triggers codegen and test invalidation. Local dependencies are
stored separately as **pubLocalDeps** and **implLocalDeps** lists of `Name`s.
Those local edges are folded into the computed hash during compilation, and
they also give selective consumers such as DBP a cached local dependency graph.

Mutually recursive groups are hashed as a unit to avoid fixed-point iteration.
We compute `selfPubHash`/`selfImplHash` for each unit, compute a `groupHash`
from the sorted self-hashes plus all external dependency hashes of the group,
and then finalize each unit as `H(selfHash || groupHash || unitKey)`.

`readNameHashes` returns a module's per-name hashes in deterministic name
order. See [Interface caches](interface_caches.md) for the exact LMDB key
layout.

```
version
meta              -- source metadata + module src/pub/impl hashes
imports
name-hash/*       -- per-name src/pub/impl hash + dep snapshots
roots
tests
docstring
```

Fresh front passes also write HTML docs for normal project builds. To avoid
running `DocPrinter` on huge modules, build-time doc output is skipped when the
module records more than 10000 top-level names; `.tydb` and compilation still
proceed. The documentation index leaves those oversized modules visible without
linking to missing module pages. The explicit `acton doc FILE.act` command still
prints docs for the requested file.

A front pass without doc output still writes the `.tydb`: `acton build
--skip-build`, `acton test list`, the LSP server and the build of a dependency
project all skip the pages. The next build then finds the module cached while
its page shows an earlier version, or is missing. So the first line of a page is
an HTML comment with a tag, the hash of the module's source hash, which covers
the docstrings, and its pub hash, which covers the signatures, followed by the
compiler stamp. A module that the front stage brings up to date without its
front passes (an impl refresh, a codegen refresh or plain reuse) compares the
tag of its page with the one its `.tydb` header gives (`docRefresh`). When the
page is missing, carries another tag, or another or newer build of the compiler
wrote it, the page is written again in the background, from the interface the
`.tydb` stores and the source parsed again, in the scope the front passes check
the module in, without checking the module again. `--verbose` reports it:

```text
  Fresh a: using cached .tydb
  Doc a: page out of date {doc page fad55488 -> c13e1d47}
```

## Deferred Back Passes

DBP is decided when a module's front stage finishes. Every module's back passes
are deferred, except those of:

- `__builtin__`
- a module with `NotImplemented` bodies, whose code a hand-written `.ext.c`
  provides
- a module at a library boundary (below)
- every module under `--only-build`, under alternate-output modes such as
  `--cgen`/`--hgen`, or under `--no-dbp`

A module whose back passes are not deferred is generated whole.

A module at a library boundary is a member of a library declared in `Build.act`
that is imported by any module outside that same library. The library boundary
needs the full generated C/H surface, so that module is compiled normally.
Internal library modules remain DBP-eligible.

When a fresh front pass produces a DBP candidate, the scheduler stores a
`DeferredBackJob` instead of queueing a normal `BackJob`. Cached `TyTask`
modules can also register deferred jobs on later builds. This matters because
the provider source and implementation hash can stay unchanged while a consumer
starts selecting a different provider name. The scheduler also records
interested names in an `InterestMap`. Fresh front results contribute the
external dependency facts they just computed; cached modules reconstruct the
same interest set from the `.tydb` dependency rows.

Deferred jobs do not wait for every front pass in the project. Their wait set is
the reverse dependency closure of the deferred module among the modules the run
compiles. After each front stage completes, the scheduler flushes any deferred
job whose wait set is now contained in the completed-front set. If a module in
that wait set fails front passes, the normal failed-build path prevents the
deferred back job from running.

When a deferred job is ready, DBP reads the module's root actor names from its
`.tydb`, then the `name-hash` rows of the names it selects, one name at a time,
without decoding the typed module. The selection seeds are:

- the collected `InterestMap` names for the module
- the module's root actor names

An empty interested-name set is valid; if the module has no root actors, DBP can
produce an effectively empty module body. DBP maps derived names back to their
owning top-level name, closes over `nhPubLocalDeps` and `nhImplLocalDeps`, and
uses exact `readExtensionsByClass` / `readExtensionsByProtocol`
lookups to keep top-level extension declarations required by selected classes
or protocols. Selection metadata that cannot map a seed, local dependency, or
extension back to a top-level name is a compiler error. DBP never falls back to
full-module compilation: a module that contains `NotImplemented`/native hooks
is not deferred in the first place.

Pruning happens on the typed module immediately before the normal back-pass
chain. Selected declarations, signatures, assignments, and compiler-introduced
`VarAssign` statements are retained by their bound top-level names; `Pass` is
dropped. Other source-level top-level statements are not a fallback case because
the parser rejects them before type checking.

Before reading any statement, DBP computes the codegen tag from the module's
`.tydb` header and the sorted selected top-level names. If the existing `.c`
and `.closures.inc` already carry that tag in their generated "Acton impl hash"
line, the `.h` carries the tag that the `.c` records, this build of the
compiler wrote them, and neither the `.c` nor the `.closures.inc` is older than
the compiler binary, the deferred job is skipped. Otherwise DBP reads only the
statements of the selected names (`readSelectedModule`), prunes them to the
selected top-level declarations, runs the normal back-pass chain, and writes the
`.c` and the `.closures.inc` tagged with that codegen tag, and the `.h` when its
text changed. A `.tydb` written by the same
compiler version always has those statement rows, so when they cannot be read
the file is corrupt, and the build stops with an internal error instead of
reading the whole module. A later consumer change that selects a different
provider name makes the provider's DBP output stale even when the provider
source did not change.

A source change always re-runs front passes for that module. Downstream modules
compare recorded `pubDeps` against current provider hashes; any delta triggers
re-typechecking. Separately, `implDeps` changes cause back passes and tests to
rerun even when public hashes are stable. Missing or stale `.c`, `.h` or
`.closures.inc` outputs also force back passes so codegen can recover. To see
the decisions, run `acton build --verbose`. It prints why a module is stale or
fresh, how the public name hashes of a module that ran its front passes changed
(`Hash deltas`), and the selection of each deferred back job (`DBP`), with every
hash shortened to its first 8 hex digits. The transcripts below come from a
project named `proj` with the modules `a` and `b`. They show only those lines:
the progress, timing and dependency lines are left out.

**Implementation-only change.** Before:

```acton
# a.act
def foo(x: int) -> int:
    return x + 1

# b.act
import a

def bar() -> int:
    return a.foo(1)
```

After changing only the body of `foo`:

```acton
# a.act
def foo(x: int) -> int:
    return x + 2
```

`srcHash(foo)` and `implHash(foo)` change while `pubHash(foo)` does not.
Downstream modules keep their public hashes stable, so they do not re-typecheck
but they do rerun back passes:

```text
$ acton build --verbose
  Stale a: source changed
  Hash deltas a: ~foo{src 0282e373 -> 57c201c1, impl 9bf0a12c -> 6db1246d}
  Stale b: impl changes in proj.a.foo 9bf0a12c → 6db1246d (used by bar)
  DBP a: total names 1, interested names 1, root names 0, selected closure 1, generated code out of date
  DBP b: total names 1, interested names 0, root names 0, selected closure 0, generated code out of date
```

`b` computes its impl hashes again from its stored rows. Both deferred back
jobs find their generated code out of date, because their codegen tags cover
their modules' impl hashes. The job of `a` selects `foo`, the one name `b`
uses, and writes `a.c` again but leaves `a.h` as it is, since its text is the
same. `b` has neither dependents nor root actors, so its job selects no names.

**Public signature change.** Before:

```acton
# a.act
def foo(x: int) -> int:
    return x + 1

# b.act
import a

def bar() -> int:
    return a.foo(1)
```

After changing the signature of `foo`, with `b.act` unchanged:

```acton
# a.act
def foo(x: int, y: int = 1) -> int:
    return x + y
```

Here `pubHash(foo)` changes, so `modulePubHash(a)` changes and any dependent
that recorded `a.foo` in `pubDeps` must re-typecheck. The call `a.foo(1)` in
`b` still type-checks:

```text
$ acton build --verbose
  Stale a: source changed
  Hash deltas a: ~foo{src 0282e373 -> 1970c99d, pub 806d27dd -> 8059e53d, impl 9bf0a12c -> 25383777}
  Stale b: pub changes in proj.a.foo 806d27dd → 8059e53d (used by bar)
  Hash deltas b: ~bar{impl 228413ba -> a779705a}
  DBP a: total names 1, interested names 1, root names 0, selected closure 1, generated code out of date
  DBP b: total names 1, interested names 0, root names 0, selected closure 0, generated code out of date
```

The impl hash of `bar` covers the impl hash of `foo`, so it changes too. Its
pub hash stays the same, because the signature of `bar` uses no name of `a`.

**Codegen-only staleness.** If generated output is missing or mismatched, we
rerun back passes even when hashes are unchanged. For example, deleting the
generated files of `b` makes its deferred back job generate them again:

```text
$ rm out/types/proj/b.c out/types/proj/b.h out/types/proj/b.closures.inc
$ acton build --verbose
  Fresh a: using cached .tydb
  Fresh b: using cached .tydb
  DBP a: total names 1, interested names 1, root names 0, selected closure 1, generated code up to date
  DBP b: total names 1, interested names 0, root names 0, selected closure 0, generated code out of date
```

A module whose code is generated whole, for example every module under
`--no-dbp`, compares the tags of its `.c` and `.closures.inc` with its codegen
tag, and the tag of its `.h` with the one its `.c` records, in its front stage
and reports a mismatch as a stale reason. After a `--no-dbp` build, the same
deletion shows:

```text
$ rm out/types/proj/b.c out/types/proj/b.h out/types/proj/b.closures.inc
$ acton build --verbose --no-dbp
  Fresh a: using cached .tydb
  Stale b: generated code out of date {codegen c missing -> 176042b0, h missing -> any, support missing -> 176042b0}
```

With the `.c` gone, no tag is known for the `.h`, so any tag would do, but a
missing `.h` carries none.

**Non-propagating change.** Adding an unused import changes the module source
bytes, so the module itself re-runs front passes, but downstream modules do
not re-typecheck because no public hashes changed:

```acton
# before (b.act)
import a

def bar() -> int:
    return a.foo()

# after (b.act)
import a
import testing  # unused

def bar() -> int:
    return a.foo()
```

Known limitations include the lack of incremental typechecking within a
module, coarse class/actor impl hashes, and docstrings affecting `implHash`.
