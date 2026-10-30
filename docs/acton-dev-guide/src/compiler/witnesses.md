# Witness sharing

Implementation lives in `compiler/lib/src/Acton/WitnessSharing.hs`. It runs at
the end of the boxing pass, `doBoxing` in `compiler/lib/src/Acton/Boxing.hs`.

A protocol witness is an ordinary object that carries the methods of one
protocol for one type. The type checker inserts witness constructions wherever
generic code needs them. For example, comparing two `list[list[int]]` values
passes a witness built as

```text
EqD_list(EqD_list(OrdD_int))
```

Witness classes such as `EqD_list` are generated from protocol and extension
declarations. A witness has no state of its own besides the witnesses it was
built from, and programs cannot observe its identity. Two constructions with
the same constructor, type arguments and argument witnesses are
interchangeable, so the compiler can build one object and use it everywhere.
Witness sharing does that.

## The rule

What happens to a construction depends only on what it is built from:

- **Closed.** Built from constructors, static witnesses and other closed
  constructions only. It is built once, during module initialization, into a
  module-level global. Every construction with the same spelling, in any
  function, method or module-level statement of the module, reads that global.
- **Parameter-dependent.** Built from a witness parameter of the enclosing
  function, as in generic code. Each call can pass a different witness, so the
  construction cannot be built in advance. Repeated constructions within one
  call of the function share one local variable.
- **Untracked.** Built from a value the compiler does not follow, such as a
  local variable or the result of an ordinary function call. It is left as it
  is.

Types are erased at runtime, so a type variable is part of the spelling and
nothing more. A construction built from type arguments alone, such as
`EqD_Bag[T]()` in a generic function, is closed: every call would build the
same object.

## Where it runs

Sharing runs on the boxed module, after normalization, deactorization, CPS
and lambda lifting. At that point every function is a top-level function or a
method, and `Let` expressions, lambdas and comprehensions have been replaced
by statements and functions. Boxing has also removed witness equations whose
witnesses are not used, so sharing never brings back a construction that is
not needed.

`doBoxing` runs two steps:

1. `shareWitnesses` shares repeated constructions within each function.
2. `poolWitnesses` lifts closed constructions to module-level globals.

Both steps start from a registry, built for the whole module by
`buildRegistry` and for a single function by `buildFunctionRegistry`. It
records every witness construction site, and describes each one as a recipe:
the constructor, its type arguments and the recipes of its argument witnesses,
together with what the recipe captures (witness parameters or untracked
values). The lifetime of a recipe, closed, parameter-dependent or untracked,
follows from its captures.

The registry is built from the module itself, with the module's own names in
scope. Static witnesses, the preallocated C objects of `__builtin__` such as
`B_OrdD_intG_witness`, are recognized and never constructed again.

## Spellings

Both steps find construction sites by their spelling within one declaration.
Equal spellings can read different bindings. A default argument expanded at
several call sites assigns its witness names once per expansion, with the
types of each call. Such constructions still denote interchangeable witnesses.
A spelling includes the construction's type arguments, which fix the types of
the witnesses it is built from, and witnesses of the same protocol and type
are interchangeable.

Different declarations can use the same name for different witnesses. A
lambda-lifted helper takes as a parameter a witness that the enclosing
function built itself, under the same name. Sites are therefore indexed per
declaration.

## Module pooling

The pool is a list of assignments placed before the module's other
statements. Each closed witness is built once, into a global. When the module
already assigns the witness to a module-level binding of the constructor's
concrete type, that binding is the global. Otherwise the global is a new one
named `W_<n>module`. The module's other module-level bindings of the witness
keep their names and are assigned the global, as aliases. For example:

```text
# before
def f(x: list[list[int]]) -> bool:
    return x == x
def g(x: list[list[int]]) -> bool:
    return x != x

# after, schematically
W_1module = OrdD_int_witness
W_2module = EqD_list(W_1module)
W_3module = EqD_list(W_2module)
def f(x): return W_3module.__eq__(x, x)
def g(x): return W_3module.__ne__(x, x)
```

The globals keep the concrete type of their constructor, so that code can read
the super-witness fields that a concrete witness class embeds.

### Initialization order

A module's initialization function initializes its imports and fills in its
class tables before it runs the module's statements. The pool is the first of
those statements. Pooled witnesses can therefore be built from imported
witnesses and objects of the module's own classes, and every module-level
statement, and every function it calls, finds them built. See
[Module initialization](../runtime/module_init.md) for the full order.

Within the pool, each witness must be built after everything it reads. A
witness reads the witnesses it is built from. Constructing an object of one of
the module's classes also runs that class's initializer. The initializer of a
witness class stores its arguments, calls its parent's initializer and
constructs its sibling witnesses. The module state it reads is:

- the module's witnesses, read by name,
- the initializers of the module's other classes, through parent initializer
  calls and sibling constructions,
- the pooled witnesses that replace closed constructions inside it.

The pool orders a graph whose nodes are the pooled recipes and the
initializers of the module's classes. Module-level witnesses that an
initializer reads are moved into the pool, so they are assigned before the
witnesses whose construction reads them. This happens, for example, when an
extension needs a witness for a type built from its own type variable:

```text
# module m, with Bag[T] and its Eq extension in module bag
extension Box[T] (Container[bag.Bag[T]]): ...

# the pool of m, schematically
W_1module = bag.EqD_Bag[T]()
W_Container_13 = W_1module      # read by ContainerD_Box.__init__
W_2module = ContainerD_Box[int]()
```

Initializers may depend on each other in a cycle. Two extensions whose
witnesses need each other, such as `Foo (Keyed[Bar])` and `Bar (Keyed[Foo])`,
have initializers that each construct the other with themselves as an
argument. The pool orders the graph by its strongly connected components, so
such a cycle is fine. A cycle that passes through a pooled witness would need
that witness for its own construction. That, and an initializer that reads
any other module state, contradicts what generated witness classes do and is
reported as an internal compiler error.

## Function-local sharing

`shareFunction` runs on every function and method except initializers, whose
receiver is still being filled in. It considers the constructions in the
function's own body that are closed or that depend on the function's witness
parameters. A construction that depends on an optional witness
parameter is left alone, since the parameter can be `None`.

A recipe used at least twice gets a local variable, `W_shared` for the first
one in a function and `W_<n>shared` after it, assigned just before the first
statement that always evaluates it. Later uses read the variable where that
assignment is guaranteed to have run:

- A variable assigned outside a branch or loop can be used inside it.
- A variable assigned inside a branch or loop body is not available after it.
- The arms of a conditional expression, the right operand of `and` and `or`,
  and the conditions of `elif` branches can use an existing variable but do
  not create one.

A variable that ends up used only once is inlined again. Closed constructions
shared this way are pooled afterwards, so the local variable becomes an alias
of a module global.

## Statistics

`acton --witness-stats` prints one JSON line per compiled module, taken after
sharing:

```json
{"functions_closed":0,"functions_parameter_dependent":2,"functions_untracked":0,"module":"m","module_level":3}
```

`module_level` counts the constructions at module level, which run once.
The `functions_*` fields count the constructions left in functions and
methods, which run on every call, by what they are built from. A closed
construction left in a function points to a missed opportunity. Only modules
that the build actually compiles are reported. A module whose back passes wait
for its dependents generates only the names they use, and the report counts
only those.

## Not shared yet

- Constructions in a nested function that the lambda lifter turned into a
  helper with witness parameters, where the enclosing function passes closed
  witnesses.
- Witnesses that a witness method derives from its own fields, such as the
  element witness of `OrdD_list.__eq__`. They could be built once in the
  witness's initializer.
- Witnesses that a generic function derives from its witness parameters. The
  caller could pass them, built once at a monomorphic call site.
- Constructions that differ only in their type arguments, such as
  `bag.EqD_Bag[T]()` and `bag.EqD_Bag[int]()`, get separate globals.
- Each module has its own pool, so equal witnesses in different modules are
  separate objects.
