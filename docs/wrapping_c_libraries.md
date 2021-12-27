# Wrapping C libraries in Acton

This document attempts to provide a brief introduction to integration C libraries in Acton.

Acton is a compiled language and C is used as an intermediate representation. The Acton syntax that you're familiar with will be compiled into C and from there a C compiler will turn that into machine code. This makes it comparatively simple to integrate C libraries into Acton; we "just" need to write some C that looks the way that the Acton compiler would have written it!

We will do that here by using an implementation of `time.time()` as an example. `time` is a module with a single free function (not in an actor or a class method) called `time()`.

The Acton compiler, `actonc`, renders somewhat funny looking C code, quite far from what one might consider idiomatic C. Rather than attempting to write this by hand, the easiest way to get started is to write an Acton program, let `actonc` generate the C code and then replace the body of the various generated C functions with the relevant content.

Acton is a statically typed language with an advanced type inferencer. When we are manually implementing C functions, the type inferencer, which works on an Acton syntax level, is unable to do its work. Thus, we must provide a type signature for our function. In order to generate surrounding code, we also write a simple dummy function.

This is `modules/time.act`:
```Acton
time : () -> int

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
$int time$$time ();
void time$$__init__ ();
```

`time.c`:
```c
#include "modules/time.h"
$int time$$time () {
    $Number w$4 = (($Number)$Integral$int$new());
    return w$4->$class->__fromatom__(w$4, (($atom)to$int(3)));
}
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
}
```

Now all we have to do is replace the content of the function `$int time$$time` with our stuff!

Updated `time.c`:
```c
#include "time/time.h"
$int time$$time () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        perror("clock_gettime");
        exit(EXIT_FAILURE);
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
}
```

We can now reduce `modules/time.act` by removing the function definition of `time()` and just leaving the type signature!

`modules/time.act`:
```Acton
time : () -> int
```

Compile it with `actonc time.act --stub` and `modules/time.ty` will be created with type signatures. Update the Makefile to produce a time.o and add it to libActon.a. Here are the relevant make targets that are added or updated for the new time module:
```Makefile

modules/time.h: time/time.h
	cp $< $@

modules/time.ty: modules/time.act modules/time.h actonc
	$(ACTONC) $< --stub

lib/libActon.a: builtin/builtin.o builtin/env.o math/math.o numpy/numpy.o rts/empty.o rts/rts.o time/time.o

# /time -------------------------------------------------
MODULES += time/time.o
time/time.o: time/time.c time/time.h
	cc $(CFLAGS) -I. -c $< -o$@

```

Recompile the rts with `make rts` and everything should be ready to go. We can use it from an Acton program like so:

`examples/time.act`:
```Acton
import time

actor main(env):
    print(time.time())
    await async env.exit(0)
```

```shell
$ actonc --root main examples/time.act
$ examples/time
1628925272624772012
$
```

The Acton RTS runs multiple concurrent threads (per default one per CPU thread) that runs actors and as such it is of utmost importance that the C libraries used are thread safe.
