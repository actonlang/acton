# Imports and Environments

This note explains how imports move through the compiler today. The important
distinction is that there are two related but different mechanisms:

- graph discovery in `Acton.Compile`, which decides module ordering and cache
  reuse
- environment materialization in `Acton.Env`, which makes imported modules
  available to kinds/type checking

If those two are conflated, it becomes hard to reason about missing imports,
stale `.tydb` reuse, or why a module can be present in the scheduler but still
need more imports loaded into the active environment.

## Environment layers

There are three environment layers worth keeping separate when discussing the
compiler.

### Base environment

`Acton.Env.initEnv` constructs the base `Env0`. It always contains `prim`, and
in normal compilation it also loads `__builtin__` from `__builtin__.tydb`.

This base environment is not tied to any one module. It is the starting point
for later compilation work.

### Shared module cache

During a compile run, the scheduler keeps an accumulated environment snapshot of
finished modules. In `Acton.Compile.compileTasks`, each completed module extends
that snapshot with:

```haskell
Acton.Env.addModuleInfo (tkMod keyDone) (frontResultModuleInfo (tkMod keyDone) fr) envAcc
```

This shared cache stores loaded module interfaces in
`modules :: Map ModName ModuleInfo`. It does not store the current module's
local names, and it should not be thought of as "the current type-checker env".
It is a shared cache of interfaces that later tasks start from.

`ModuleInfo` is the only Env-side representation of dependency modules. A local
module backs it with an in-memory `TEnv`; an imported module backs it with a
read-only `.tydb` handle plus compact metadata. Normal compiler code should ask
`ModuleInfo` for specific names or specific indexes, not for a whole imported
`TEnv`.

### Current-module import overlay

When the compiler is about to kinds/type-check a module, it takes the shared
snapshot and overlays the module's imports onto it with `Acton.Env.mkEnv`.

`mkEnv` does not add the current module's own declarations. It only processes
the module's `import` statements. Later, the type checker sets `thismod` and
defines the module's own top-level names in a more specialized environment used
within the pass.

So the rough model is:

1. start from the shared module cache
2. load the imported modules needed for this module
3. type-check the module and derive its interface
4. add that interface back into the shared module cache

## Scheduler discovery versus import loading

The scheduler in `Acton.Compile` does not eagerly materialize every imported
module into the active environment.

Instead it does header-first discovery:

- `readModuleTask` reads the `.tydb` header when possible and produces a cheap
  `TyTask`
- otherwise it parses the source and produces an `ActonTask`
- `buildGlobalTasks` uses the task's direct imports to build provider edges

Those provider edges only cover imports that can be resolved to modules inside
the discovered project graph. This is deliberate: they are scheduler edges, not
the full transitive import closure of the final type-checker environment.

This matters for standard library modules and any other imports that are not
represented as in-graph provider tasks. Such modules still have to be made
available later through `Acton.Env`.

## When `.tydb` headers are read

The compiler reads `.tydb` data in three different modes.

### Header-only reads

Header summary reads are used for cheap decisions:

- module freshness
- direct import names for graph construction
- module public and implementation hashes and the stored name count
- roots, tests, and docstrings

This is the path used by `readModuleTask` and the various cache lookups in
`Acton.Compile`. Per-name dependency hashes are no longer part of the header
read; stale checks and DBP selection read exact `name-hash` rows on demand.

### Full `.tydb` reads

Full reads are used when the compiler needs the full typed payload, not for
normal imported-name lookup:

- implementation-hash refresh
- codegen refresh
- DBP back-pass execution after the selected codegen hash is stale
- tools that intentionally inspect a whole interface
- completion fallback when the normal import environment cannot be restored

Reading a full `.tydb` does not by itself reconstruct the import closure in the
active environment. It only gives the caller the stored interface and typed
module payload.

DBP interest is also scheduler metadata, not an import overlay. Each completed
front result contributes external dependency names into an `InterestMap`; for
cached modules those names are reconstructed from the `.tydb` dependency rows.
DBP later uses that map to prune a provider's typed module. That does not make
the selected provider names available to the active type-checker environment;
imports still become type-checker bindings only through `mkEnv`/`doImp`.

### Selective interface reads

Normal imported-module lookup uses selective reads. `doImp` opens the imported
module's `.tydb` with `InterfaceFiles.openInterfaceDB`, reads only imports and
docstring, then installs a `ModuleInfo` shell in `modules`.

The shell has a pure-looking lookup function. `tryQName`, selected imports, and
generated-name lookup call that function with one `Name`; the implementation
runs `readInterfaceDBNameInfoMaybe` for that name and memoizes the result.
Read/decode failure aborts the compilation like any other corrupt interface
cache error.

Passes that rewrite imported names do not materialize the module either. They
wrap `moduleLookupHName` with their conversion function (`convertModules`), so
Normalizer, Converter, Deactorizer, CPS, and LambdaLifter convert only names
that are actually demanded. Generated names map back to their source name
before lookup when necessary.

Solver queries go through narrow per-module indexes: attribute owners,
descendants, and extension witnesses keyed by protocol or by type. The
attribute indexes record attributes where they are declared; `allConAttr` and
`allPConAttr` complete inherited owners with the descendants of each declaring
constructor, which preserves the ancestry-aware semantics of the old full
scan. All transitive imports are consulted with keyed reads, so query cost is
proportional to the number of imported modules, never to their size.

Broad enumeration stays explicit. `from module import *` walks
`modulePublicNames` and forces one lookup per importable public name.
Completion, documentation, and debug paths may also choose broad reads, but
those paths are separate from normal qualified-name and selected-import
lookup.

## How transitive imports become available

The transitive import closure is reconstructed through `Acton.Env.mkEnv`,
`impModule`, `doImp`, and `subImp`.

When a module imports another module:

1. `mkEnv` walks the AST import list
2. `impModule` delegates to `doImp`
3. `doImp` first checks whether the imported module is already in `modules`
4. if it is loaded, `doImp` follows the recorded `moduleImports` list
5. otherwise, `doImp` opens the imported module's `.tydb`, reads its import
   metadata, and follows the stored imports
6. only then does it return the imported module interface shell

This is the mechanism that turns a direct import into a transitive environment
closure suitable for kinds and type checking.

An important invariant is that a cached direct module must still restore its own
recorded imports. `ModuleInfo` stores the imported module's import list, so a
module already present in `modules` can restore its transitive imports without
any `.tydb` read.

When the module is not present in `modules`, `doImp` falls back to the `.tydb`
cache on the search path, opens the module metadata, recursively materializes
its imports, and then adds the module shell to the shared cache.

The boundary is important: `.tydb` is a persistent cache and cross-process
interface artifact, not an IPC mechanism between front stages in one compiler
run. If a module was just compiled by the current process, dependent front
passes should use the front result already in memory. This keeps front-stage
scheduling independent from any asynchronous `.tydb` writer still flushing that
same interface to disk.

Without restoring the transitive imports, the compiler can end up in a state
where:

- the direct module is present in the shared module cache
- the direct module's transitive imports are not
- later type checking sees the direct module but fails when its public interface
  refers to a transitive dependency

## Practical debugging rules

When debugging import failures, it helps to ask which layer is failing:

- Is the module missing from graph discovery? Then look at `readModuleTask`,
  `.tydb` headers, and `buildGlobalTasks`.
- Is the module outside the project graph but still needed for type checking?
  Then look at `mkEnv` and `doImp`.
- Is a cached module present but its transitives missing? Then the issue is in
  environment materialization, not scheduler ordering.
- Is a full `.tydb` being read during normal import lookup? Then look for a
  caller that asks for broad enumeration instead of `tryQName` or
  `moduleLookupName`.

## Related pages

- [Compiler overview](index.md)
- [Incremental compilation](incremental_compilation.md)
- [Interface caches](interface_caches.md)
- [Type check](passes/type_check.md)
