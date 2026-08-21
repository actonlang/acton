# Compile-time performance tests

This directory contains manual tests that measure compiler performance on
synthetic projects. No test here runs in `make test` or in CI. The
interesting configurations are deliberately slow on unfixed compilers, and
the results change from machine to machine. Use these tests to:

- show a compile-time slowdown
- compare two compiler builds on the same input
- get a starting point for profiling `actonc`

## attr_lookup_repro: attribute-owner enumeration in the solver

`gen.py` and `run.sh` reproduce a typechecker slowdown seen in a user project.
The `test_service_discovery` module became several times slower to typecheck
after acton-yang#472 ("Use generic MKeyedList with tuple keys for adata lists").
The module imports a large transitive closure of generated YANG-style data
classes.

**The main finding: the cause is in the compiler, not in the acton-yang
change.** acton-yang#472 changed two things. Lists became subclasses of a
generic base class, and compound keys became named tuples. Neither
ingredient causes the slowdown. The generic base class has no measurable
effect at all. Compound keys cost the same moderate factor on the old and
the fixed compiler, with both key representations. The section "Generics
vs tuple keys" below has the numbers. The cause is that the typechecker
recomputed large attribute-owner candidate lists on every solver step.
Almost any change to this quantity of generated code could have made that
visible. Branch `typecheck-env-caching` removes the recomputation.

### The mechanism

The constraint solver ranks each `Sel`/`Mut` constraint whose receiver is
still a unification variable. To rank it, the solver collects every
candidate type that has the selected attribute name (`allClassAttr` and
`allProtoAttr` in `compiler/lib/src/Acton/Solver.hs`). Before the fix, each
of these calls computed two things again from scratch:

- the transitive import closure (`transitiveImports`, then
  `lookupModuleInfo` for each module)
- `importedConAttr` / `importedProtoAttr`: every imported class that
  declares the attribute name, plus every descendant of every declaring
  class (`moduleDescendants` for each owner in each module), made unique
  with a pairwise `nubBy`

The solver ranks every remaining constraint at every iteration. A function
with S attribute selections therefore computes these candidate lists up to
S x iterations times. Generated YANG data code also makes each candidate
list large. Hundreds of classes share the method names `get`, `create`,
`copy` and `to_gdata`, and the YANG leaf names `name`, `address`, `port`
and so on. With the acton-yang#472 shape, every generated list class is
also a descendant of the one generic `MKeyedList` base that declares
`.get()`.

The fix is on branch `typecheck-env-caching`. It caches the
transitive-import `ModuleInfo` list in the environment. It memoizes
`importedConAttr` / `importedProtoAttr` per attribute name. It computes
both again only when the import closure changes. It also makes
`Ord ModName` compare name components directly, and it removes a no-op
string copy in `nstr`. The scans above did many comparisons through these
two functions.

### The generated project

```
yadata.act        The MNode / MList / MKeyedList base classes and the
                  Iterable / Indexed extensions. Copied from the relevant
                  part of yang.adata.
schema_NNN.act    Generated "YANG schema" modules. Each list gets an MNode
                  entry class and a keyed-list class. The entry classes
                  take their leaf attributes from a shared name pool. Each
                  module imports its predecessor, so the consumer sees a
                  deep transitive closure.
consume.act       A consumer in the style of test_service_discovery. It
                  iterates over keyed lists, filters on leaf values, and
                  copies entries into another tree with .get() and
                  .create().
```

The default configuration has the same shape as the user project: few
modules, each with a large number of classes, like generated device schema
modules. The default is 6 schema modules x 2 containers x 20 lists =
240 keyed lists (about 500 classes, about 2300 lines per module). The
class count (`--lists`) is the dimension that matters most here. See
"Module count vs module size" below for the measurements that separate the
two dimensions.

`--style keyed` (the default) makes each list a subclass of the generic
`MKeyedList[K, T]`, as acton-yang does after #472. `--style old` makes
each list a subclass of `MList` with a per-class `get()` and a per-class
`Indexed` extension, as acton-yang did before #472.

`--keys scalar` (the default) gives each list a `str` key. `--keys
compound` gives each list a (name, port) key. With `--style keyed` the
compound key is a named tuple, as acton-yang uses after #472. With
`--style old` the compound `get()`/`create()` take one argument per key
leaf, and there is no `Indexed` extension, as acton-yang generated before
#472.

These two knobs isolate the two ingredients of the acton-yang#472 change.
See "Generics vs tuple keys" below for the result: neither ingredient
causes the slowdown. The test shows a compiler-side problem, not a problem
in the acton-yang change itself.

### Usage

```sh
# measure the compiler in ../../dist/bin/acton
./run.sh

# more classes in the same few modules (the dimension that hurts)
./run.sh --lists 40

# compare with a second compiler build
ACTON_BASELINE=/path/to/other/dist/bin/acton ./run.sh
```

The metric is the typecheck time of `consume.act` on a rebuild after a
comment-only edit. This is the edit-recompile cycle of the consumer module.
All schema modules are already compiled, and the compiler reads them from
`.tydb`. `run.sh` also prints the wall time of the initial full build.

To build a pre-fix compiler for the comparison:

```sh
git worktree add /tmp/acton-baseline <commit>
cd /tmp/acton-baseline
ln -s /path/to/main/checkout/deps-download deps-download   # reuse dep tarballs
make dist/bin/acton dist/base dist/std -j8
```

### Reference numbers

Measured on Apple M-series macOS, 2026-08-21. `branch` is
`typecheck-env-caching` (fc930bd0). `baseline` is its merge-base with main
(69bc98af). The consumer has 8 functions x 12 blocks, about 1700 lines.
Times are the `consume.act` rebuild typecheck unless noted.

| configuration (6 modules)                | baseline | branch | speedup |
|------------------------------------------|----------|--------|---------|
| default: 240 lists (~500 classes)        | 56.5 s   | 14.6 s | 3.9x    |
| --lists 40: 480 lists (~980 classes)     | 148.1 s  | 51.3 s | 2.9x    |
| full build, default shape                | 37 s     | 16 s   | 2.3x    |

In this few-modules, many-classes shape, the remaining branch cost is the
class-count term. The speedup is therefore 3-4x here. The module-count
term, which the branch removes completely, is small at 6 modules. Wide
closures with many modules show speedups up to about 9x. Note that the
branch time still grows superlinearly with the class count: 14.6 s to
51.3 s for 2x the classes.

The rebuild typecheck of `consume.act` is slower than the same typecheck
during a full build. The rebuild reads the imported modules from `.tydb`,
and the repeated candidate-list computations then also amplify the
on-demand interface reads.

### Module count vs module size

To separate the two dimensions, keep the total class count constant and
change the number of modules that contain it. Then keep the module count
constant and change the per-module size. Times are `consume.act` rebuild
typecheck seconds.

240 lists in total, divided over M modules (`--modules M --lists L`):

| M x L(per container) | baseline | branch |
|----------------------|----------|--------|
| 5 x 24               | 51.2     | 14.6   |
| 10 x 12              | 66.1     | 13.4   |
| 20 x 6               | 97.7     | 12.4   |
| 40 x 3               | 167.5    | 13.0   |

10 modules, with increasing per-module size:

| total lists | baseline | branch |
|-------------|----------|--------|
| 120         | 23.8     | 4.1    |
| 240         | 66.1     | 13.4   |
| 480         | 170.4    | 44.7   |

On the baseline, both dimensions matter. At a constant total class count,
the time closely fits T = 34.5 s + 3.33 s x modules. The linear per-module
term comes from three sources, each paid on every ranking call:

- the `transitiveImports` walk
- the M per-module index lookups behind every owner scan
- the owners x modules `moduleDescendants` completion

The constant part is the class-count base: the owner-list sizes and the
pairwise `nubBy`. From about 10 modules up, the per-module term dominates
on the baseline.

The branch removes the per-module term completely. Its time is flat from
5 to 40 modules at a constant total. The branch also makes the class-count
term about 4x smaller. The remaining time scales only with the total class
count. The memoized owner lists are computed once per attribute name, but
the solver still traverses them at each ranking step. Improvement beyond
the caching fix is therefore possible. To make this test heavier, increase
the class count (`--lists`), not the module count.

### Generics vs tuple keys

acton-yang#472 changed two things in the generated code. Lists became
subclasses of a generic base class (`MKeyedList[K, T]`). Compound keys
became named tuples. The `--style` and `--keys` knobs test each ingredient
separately. All 8 cells below come from one measurement session, at the
default shape. Times are `consume.act` rebuild typecheck seconds.
(Run-to-run variance is about 5 percent, so the scalar cells differ a
little from the reference table above.)

| style x keys          | baseline | branch | speedup |
|-----------------------|----------|--------|---------|
| keyed, scalar         | 52.9     | 13.9   | 3.8x    |
| old, scalar           | 52.9     | 13.9   | 3.8x    |
| keyed, compound       | 78.7     | 20.5   | 3.8x    |
| old, compound         | 78.6     | 21.7   | 3.6x    |

Three conclusions follow:

- The style has no effect in any cell. The generic base class is not the
  cause of the slowdown.
- Compound keys cost about 1.5x in every cell, on both compilers. The cost
  is the same with a named tuple and with plain per-leaf arguments. The
  extra cost therefore comes from the second key leaf (more constraints
  per call site), not from the tuple representation.
- The branch gives the same speedup in every cell. The caching fix is
  independent of both acton-yang#472 ingredients.

These measurements are the basis for the main finding at the top of this
section: the primary reason for the slowdown is the compiler-side
recomputation of the attribute-owner candidate lists, and the size of
those lists.
