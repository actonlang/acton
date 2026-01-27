# Example 1 - Harmonic numbers

Even though the top level view of an Acton program is that of a
community of interacting actors, function and method definitions play
major roles in a program; typically the bulk of the program text is a
collection of `def`'s. Here is a simple function
definition, which is valid in both Python and Acton:

```py
def harmonic(n):
    sum = 0
    for i in range(1,n+1):
        sum += 1/float(i)
    return sum
```

For a given non-negative integer argument ***n***, `harmonic(n)` computes
the ***n***'th harmonic number, i.e. 1+1/2+1/3+ ... +1/***n***.
We can use this definition together with a simple actor that expects
the integer argument to `harmonic` on the command line:

```py
actor harmonicMain(env):
    print(harmonic(int(argv.env[1])))
```

We used the name `harmonicMain` for this actor just to emphasize
that there is no name convention for the root actor in Acton; instead
its name is given to the compiler. So to compile and run this program
(saved to `harmonic.act`) we do:

```shell
$ acton harmonic.act --root harmonicMain
$ ./harmonic 2
1.5
$ ./harmonic 10000
9.7876
```

Let's make the program a bit more interesting by making it
interactive, repeatedly inputting an integer from the user and
outputting the corresponding harmonic number. We have to change the
actor:

```py
actor harmonicMain2(env):
    def response(input):
        print(harmonic(int(input)))

    env.stdin_install(response)
```

This actor definition has two parts: first we have the definition of a
local method `response`, which describes how the actor should react
to one line of user `input`. Then follows the actor
initialization code, which simply installs `response` as a
callback in the environment.

Program execution starts by running the root actor's initialization
code. After that, the program is idle, waiting for
user input on `stdin`. When such
input arrives, the callback is executed, i.e. the input string is
converted to an integer, which is given as argument to
`harmonic`, the result is printed and the program is again idle. 

This program has obvious shortcomings: there is no prompt to the user,
no check that the input is actually a string that can be converted to
an integer, and no way to terminate the program except by some interrupting
signal. In spite of this, we leave the program as it is, with one
final note.
We have implicitly assumed that both the function declaration and the
actor declaration are in the same file. Of course, we could have the
function declaration in a separate library file, say
`functions.act`, and the actor in `main.act`. In that
case, we need to add a line at the beginning of the latter file,
saying

```py
import functions
```

Our main reason for using function `harmonic` as an early
example is not the mathematics, but to use it to discuss the different type
disciplines and scope rules of the two languages. Acton adopts many
static checks in order to find programming errors at compile
time rather than during execution. Some readers may prefer to proceed
to the next example at a first reading and come back to the following
discussion later.

## Typing in Python and Acton

Both Python and Acton use types as a sanity check
on computations, but in very different ways.

Python uses ***dynamic*** typing, i.e. type-checking occurs at runtime.
All values have their type attached, and before the interpreter attempts
an operation, the types of operands are checked to be compatible with
the operation. To roughly see how this works, consider the call

```py
harmonic(3)
```

The definition above ***binds*** the name `harmonic` to an
object of type `function`. This type means only that we may use the
object in a function call. Thus, parameter `n` is bound to the
integer 3, and the function body is executed. 3 will be added to 1, which is fine;
integer values can be added. The result is used as the second argument
to `range`, which also is type-correct since the builtin function
`range` has known type and expects integer arguments. In similar
style types are checked at each step until we arrive at the final value
1.8333333333333333. If we instead would try to compute 

```py
harmonic(7.5)
```

The interpreter will happily start the computation, but stop when it
sees that the value 8.5 will be used as second argument to
`range`. This does not make sense, so execution is interrupted
and a `TypeError` exception is raised.

Acton, on the other hand, uses ***static*** typing, i.e. all types are
inferred at compile time, starting from known types of all builtin
functions and objects. The type inference algorithm analyzes the
function definition and sees for example that `n+1` is second argument to
the builtin function `range`, which in Acton has the much more
informative type `(int,int) -> int`. (This is a slight
simplification; `range` actually takes ***three*** integer
arguments, where the optional third argument has default value 1.) Thus `n+1` must be an
`int`, and hence `n` must also have type `int`. In a similar
way it can conclude that the function result will be a `float`,
and hence that `harmonic` has type
`(int) -> float`, i.e. it is a function that takes
an integer argument and produces a floating point result. We note that
in a static typing discipline types are assigned to variables, not to values; type inference is done at
compile time and no values are yet computed. Acton also has a much
more expressive type language than Python. The more detailed type of
`harmonic` in Acton makes it easy to infer already at compile time
that `harmonic(7.5)` is a type error. Thus a program containing
that application is rejected and we avoid a later runtime error.

Static typing also implies that there is no need for type-checking at
runtime. This is a minor efficiency advantage, but in addition the extensive type
information available at compile time gives a strong basis for
proper compilation to machine code, a move that promises major 
gains in execution efficiency compared to Python's interpretation model.

## Scope rules in Python and Acton

To discuss this, we consider a slight variant of this function. In fact, it is an
inferior version, which we show only to be able to discuss the 
different scope rules.

```py
def harmonic2(n):
    if n>=0:
        sum = 0
        for i in range(1,n+1):
            sum += 1/float(i)
    return sum
```

This code could be the result of the following misguided
thinking: harmonic numbers only make sense for non-negative ***n***, so we should
check for that in the function body. However, the result is that
***if*** the function is called with a negative argument, the
variable `sum` has not been assigned any value before it is returned. If
we try to compute `harmonic2(-1)`, the interpreter will note,
before returning `sum`, that the variable is unbound and raise an
`UnboundLocalError`. So, this function definition is valid
Python and works as intended for non-negative arguments, but using it
with a negative argument results in an exception at runtime. The reader
should not be surprised to learn that this function definition is
rejected by the Acton compiler. In this case the problem is not with
types; instead the ***scope rules*** of Acton rule out the definition.

Scope rules define where in the program text names become bound
(i.e. acquire meaning) and in which part of the program these bindings
are valid. In Python, since `sum` does become bound in the body
(the statement `sum = 0`), the variable is, surprisingly, in
scope in the ***whole function body***, also after the `if` statement, in
spite of the fact that execution of the binding statement may not have
been executed (which is exactly what happens when ***n*** is negative).

We defer a detailed discussion of scope rules to section 3.3 below. Here we
just note that in Acton `sum` will not be in
scope in the `return` statement, for exactly the reasons hinted
at above. In general, Acton adopts scope rules that rule out
runtime errors because of unassigned variables. The problem with
`harmonic2` 
can be fixed by moving the initialisation of `sum` to before
the `if` statement, by adding an `else` clause that also
declares `sum` or, preferably, by sticking to our first definition.

In summary, Example 1 shows two function definitions, which both are
valid in Python, but which may result in runtime errors when applied.
Acton avoids these problems, in one case by discovering type problems in
an application of the function already at compile time, and in the other by
rejecting the definition altogether, since it allows also type-correct
use to give a runtime error.

Of course, we cannot expect to avoid runtime errors completely. Programs
execute in an environment where errors may be caused by external
factors: faulty sensors, failing networks, corrupt files, etc. But
Acton adopts the point of view that runtime errors which are caused not by the
external world but by programming mistakes should be avoided as far as
possible. Thus extensive static checks are performed at compile time.

In Python, numeric code is often best written using the [NumPy](https://numpy.org/) package.
Acton provides a package with some of the functionality of NumPy; to
see how this is used to express function `harmonic`, see [WIP].
