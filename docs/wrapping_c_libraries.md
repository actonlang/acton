# Wrapping C libraries in Acton

This document attempts to provide a brief introduction to using C for implementing functions and actors in Acton. The Acton stdlib is the only real use case where C is used. All other libraries and applications are implemented in pure Acton. Thus, this document is for those of you that intend to contribute to Acton's stdlib.

Acton is a compiled language and C is used as an intermediate representation. Acton programs are turned into C which in turn are compiled to machine code using a standard C compiler. This makes it conceptually straightforward to integrate C libraries into Acton; we "just" need to write C code that looks the way that the Acton compiler would have written it! The simplest way to achieve this is to first have the compiler generate some C code for us, which we can go ahead and modify to our hearts content.

C can be used to implement a whole module or individual functions and methods. Regardless to which extent C is used, the modules interface needs to be specified in Acton so that the Acton compiler has a view of the interface with types. While Acton has an advanced type inferencer, it is not able to inspect C code and reverse engineer the types used. It can infer the type based on how the module might be used, but it is much safer and thus recommended to write a type signature for all functions implemented in C.

As an example, we will create our own `time.foo()` function, which will be a duplicate of the `time.time()` function.

Start by writing the Acton declaration of the function in `stdlib/src/time.act`:
```Acton
def foo() -> float:
    """Return the current real time
    """
    return 3.14
```

`foo()` does not take any arguments and returns a float. We provide a short description of the function and write a dummy return statement. This is so that we may generate some boilerplate code.

Run `acton --cgen stdlib/src/time.act` to generate C code. The `foo()` function will be called `time$$foo` and should look something like (may change with compiler version):
```c
$float time$$foo () {
    $RealFloat w$232 = (($RealFloat)$RealFloat$float$new());
    return (($float (*) ($RealFloat, $atom))w$232->$class->__fromatom__)(w$232, (($atom)to$float(3.14)));
}
```

Change the code in `stdlib/src/time.act` to replace the function body with `NotImplemented`:
```Acton
def foo() -> float:
    """Return the current real time
    """
    NotImplemented
```
This instructs the Acton compiler to not generate C code for this function if an external definition C file exists.

Next, paste in the copied C code into the external C definition file `stdlib/src/time.ext.c`:
```c
$float time$$foo () {
    $RealFloat w$232 = (($RealFloat)$RealFloat$float$new());
    return (($float (*) ($RealFloat, $atom))w$232->$class->__fromatom__)(w$232, (($atom)to$float(3.14)));
}
```

Recompile everything and write a test program that uses the new function:
```Acton
import time
actor main(env):
    print(time.foo())
    exit(0)
```

And it should print `3.14`! We can now go ahead and modify `stdlib/src/time.ext.c` to implement the actual functionality we want, like so:
```
$float time$$foo () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001*ts.tv_nsec);
}
```

Acton values are boxed and are available in C as `$float`, `$int`, `$str` etc. You will need to convert your machine native values into Acton values.

## Considerations

- The Acton RTS runs multiple concurrent threads that run actors and as such it is of utmost importance that the C libraries used are thread safe.

- All functions in Acton are run synchronously. Never ever ever perform I/O (which might never complete) or other similar operations, instead, model your C thing as an actor. Actor methods calls are always asynchronous and thus might never return. By design, there is no way to do blocking I/O in Acton, however, since you are writing things in C, there's enough rope to hang yourself (and your Acton system)...

- Actors can be resumed if they die or are migrated from another compute node. If you keep state, make sure it survives a resumption (and implicitly the snapshotting of its state). There is a `__resume__` callback that can be used as a hook point to perform extra steps after an actor has been deserialized into memory.
