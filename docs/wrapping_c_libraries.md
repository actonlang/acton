# Wrapping C libraries in Acton

This document attempts to provide a brief introduction to integration of C libraries in Acton.

Acton is a compiled language and C is used as an intermediate representation. The Acton syntax that you're familiar with will be compiled into C and from there a C compiler will turn that into machine code. This makes it comparatively simple to integrate C libraries into Acton; we "just" need to write some C that looks the way that the Acton compiler would have written it!

We will do that here by using an implementation of `time.time()` as an example. `time` is a module with a single free function (not in an actor or a class method) called `time()`.

The Acton compiler, `actonc`, renders somewhat funny looking C code, quite far from what one might consider idiomatic C. Rather than attempting to write this by hand, the easiest way to get started is to write an Acton program, let `actonc` generate the C code from this Acton program and then replace the body of the various generated C functions with the actual functions we want.

Acton is a statically typed language with an advanced type inferencer. When we are manually implementing C functions, the type inferencer, which works on an Acton syntax level, is unable to do its work. Thus, we must provide a type signature for our function. In order to generate surrounding code, we also write a simple dummy function.

This is `stdlib/src/time.act`:
```Acton
time : () -> float

def time():
    return 3
```

It is important that the file is placed in `modules/` in order for it to be "at the root". For example, file `foo/bar.act` with a function `banana()` would become available as `foo.bar.banana()`. Since we want a top level module `time` with a function `time.time()` we have to place it directly in `modules/`. There should be a better way of accomplishing this, but this is currently the best approach.

The type signature for `time()` means it takes no argument and will return an integer.

We generate a header file using `--hgen` and the C code using `--cgen`:
```shell
~/acton/modules$ actonc time.act --hgen > ../time/time.h
~/acton/modules$ actonc time.act --cgen > ../time/time.c
```

`time.h`:
```c
#pragma once
#include "builtin/builtin.h"
#include "builtin/env.h"
#include "rts/rts.h"
$float time$$time ();
void time$$__init__ ();
```

`time.c`:
```c
#include "time.h"
$float time$$time () {
    $Number w$4 = (($Number)$Integral$int$new());
    return w$4->$class->__fromatom__(w$4, (($atom)to$float(3)));
}
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
}
```

Now all we have to do is replace the content of the function `$float time$$time` with our stuff!

Updated `time.c`:
```c
#include "time/time.h"
$float time$$time () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001*ts.tv_nsec);
}
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
}
```

We can now reduce `stdlib/src/time.act` by removing the function definition of `time()` and just leaving the type signature!

`stdlib/src/time.act`:
```Acton
time : () -> float
```

If you've placed these files in `stdlib/src` as suggested, the main Makefile will automatically pick up on them through a wildcard pattern so they will be compiled and made available.

Run `make` and everything should be ready to go. We can use it from an Acton program like so:

`examples/print_time.act`:
```Acton
import time

actor main(env):
    print(time.time())
    await async env.exit(0)
```

```shell
$ actonc --root main examples/print_time.act
$ examples/time
1.64064e+09
$
```

The Acton RTS runs multiple concurrent threads that run actors and as such it is of utmost importance that the C libraries used are thread safe.
