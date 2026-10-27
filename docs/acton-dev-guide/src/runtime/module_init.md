# Module initialization

Every compiled module has a generated initialization function. This page
defines when it runs, the order of the work it does, and the contract for the
C extension hook `__ext_init__` that it calls.

## Process start

`main()` in `base/rts/rts.c` prepares and initializes the program in this
order:

1. The GC is initialized, the command line is parsed and the event loops are
   created. No event loop runs yet.
2. The Acton allocator is pointed at libc (see
   [Memory and GC](memory.md#the-acton-allocator)).
3. The `__builtin__` module is initialized (`B___init__()`).
4. `$ROOTINIT()` calls the initialization function of the root module. It is
   generated into `out/types/<root>.root.c`. Through imports, it initializes
   every module the program uses.
5. The Acton allocator is pointed back at the GC.
6. `BOOTSTRAP()` creates the `Env` and root actors. With a database backend
   that already holds actor state, the actors are restored from it instead.
7. The worker threads start.

All module initialization therefore runs on the main thread, before any
worker thread exists, before any event loop runs and before any actor is
created.

## The initialization function

`cModuleParts` in `compiler/lib/src/Acton/CodeGen.hs` generates, for a module
whose C prefix is `M`:

```c
int M_done$ = 0;
void M___init__() {
    if (M_done$) return;
    M_done$ = 1;
    M___ext_init__();       // 1. only if the module has NotImplemented
    I___init__(); ...       // 2. each imported module, in import order
    M___init__$support();   // 3. class tables (inline when not split)
    M___init__$g0(); ...    // 4. module-level statements, in chunks
}
```

The steps are:

0. **Guard.** The flag is set before any other work. The function does its
   work at most once per process, and a call that arrives while the module is
   being initialized returns immediately.
1. **Extension hook.** `M___ext_init__()` is called if the module has
   `NotImplemented` declarations. See below.
2. **Imports.** The initialization function of each imported module is called.
   A module that another importer already initialized returns at once. Which
   module actually initializes a shared import therefore depends on the whole
   program's import graph, not on this module alone.
3. **Class tables.** The method table and base class of every class in the
   module are filled in, including actors and compiler-generated classes.
   Objects of these classes cannot be constructed before this step.
4. **Module-level statements.** The module's top-level statements run in
   source order and assign its module-level values. Besides the values written
   in the source, these include values the compiler introduces, such as
   protocol witnesses that the module's functions read. The statements are
   split into helper functions of at most 250 statements each.

## `__ext_init__`

### Definition

A module whose `.act` source contains `NotImplemented` (as the body of a
function or method, or as a module-level value) is partly implemented in C, in
a file `<module>.ext.c` next to `<module>.act`. The generated C file for the
module includes the `.ext.c` file and calls its hook as step 1 above. For a
module without `NotImplemented`, the compiler neither includes the `.ext.c`
file nor calls a hook.

The hook takes no arguments and returns nothing. Its name is the module's C
prefix followed by `___ext_init__`, for example `jsonQ___ext_init__` in
`base/src/json.ext.c` and `stdQ_jsonQ___ext_init__` in `std/src/json.ext.c`.
The commented-out skeleton in the generated `out/types/.../<module>.c` shows
the exact name. The hook must be defined: the call is unconditional, so a
missing definition is a C compile error.

### Purpose

The hook sets up the C state that the module's C functions rely on. Examples:

- initializing a C library: `xml.ext.c` calls `xmlInitParser()`
- seeding a C random number generator: `random.ext.c` calls `srand()`
- initializing C globals of the extension: `time.ext.c` reads the clock
  incarnation id

Since module initialization is single-threaded, the hook is also where to do
one-time initialization that a C library does not lock itself. libxml2 is built
without thread support, so `xmlInitParser()` must run before any worker thread
parses.

### State when the hook runs

Initialized:

- the GC and the RTS
- the `__builtin__` module

Not guaranteed to be initialized:

- the modules this module imports (one of them may already have been
  initialized through another importer, but that depends on the import graph)
- the class tables of this module
- the module-level values of this module, including those the compiler
  introduced

Also:

- The hook runs on the main thread. No worker thread exists and no event loop
  runs.
- `acton_malloc` and its siblings return libc memory, which is never freed and
  which the GC does not scan. A GC pointer stored in it does not keep its
  referent alive (see [Memory and GC](memory.md)).

### Rules

1. The hook must not depend on Acton code. It must not call any function or
   method written in Acton, from this module or from any other module, and it
   must not construct objects of Acton classes or read Acton module-level
   values.
2. The same holds for code the hook starts that can run before module
   initialization has finished, for example a thread it creates. Callbacks
   registered with an event loop are fine: the loops only start after all
   modules are initialized.

The rule follows from step 1 coming first. Acton code called from the hook can
find any of the uninitialized state listed above. That holds even for a
function that reads no module-level value in its source, because the compiler
moves values such as protocol witnesses into module-level variables. These are
only assigned in step 4.

The C functions that implement the `NotImplemented` declarations are not
covered by these rules. They are called from Acton code, which only runs after
the hook has returned.
