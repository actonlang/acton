# Integrating a C library (zlib)

This is a guide to integrating C libraries in Acton code. We will use the zlib compression library, written in C, to build an Acton module that supports zlib compression and decompression.

We will only focus on the `inflate` and `deflate` functions in zlib. They are pure functions (meaning they only take some input and return some output, they do not have any side effects like writing to some shared state), that makes them easier to integrate than anything that does I/O. While zlib does expose functions to interact with files, we don't want to reimplement file related functionality since we already have this supported by the Acton stdlib.

## Create new project

Let's start by making a new Acton project, let's call it `acton-zlib`. New projects are created with an example "Hello world" app. Let's remove it and start from scratch.
```console
acton new acton-zlib
cd acton-zlib
rm src/*
```

## Acton's low level build system - the Zig build system
The Acton compiler parses .act source code, runs through all its compilation passes with type checking, CPS conversion, lambda lifting etc and finally produces C code. Internally, Acton then uses the Zig build system to compile the generated C code to libraries and finally binary executables.

To add a C library dependency, it first needs to be buildable using the Zig build system, which means that it needs a `build.zig` file, the config file for the Zig build, somewhat similar to the CMakeLists.txt of CMake. Some projects have already adopted a `build.zig` in the upstream repo, like PCRE2 and the Boehm-Demers-Weiser GC (both of which are used by Acton). In some cases, there are forks of projects with `build.zig` added. Otherwise you will need to write one for yourself, which is usually simpler than it might first seem.

## Add the zlib C library as a Zig dependency

In the case of zlib, there is already [a repo available with a build.zig for zlib](https://github.com/allyourcodebase/zlib/). Navigate to the Tags page, find `1.3.1` and the link to the source files, i.e. `https://github.com/allyourcodebase/zlib/archive/refs/tags/1.3.1.tar.gz`.

Add it to our `acton-zlib` project:
```console
acton zig-pkg add https://github.com/allyourcodebase/zlib/archive/refs/tags/1.3.1.tar.gz zlib --artifact z
```

Note the `--artifact z` which is provided to instruct which library to link with. Headers from the zlib library, like `zlib.h`, will now become visible to C files in our project and the `z` library will be linked in with our executables. The easiest way to discover what the artifacts are called is by inspecting the `build.zig` file of the package. This particular zlib `build.zig` starts like this:

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const upstream = b.dependency("zlib", .{});
    const lib = b.addStaticLibrary(.{
        .name = "z",
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });
    lib.linkLibC();
    lib.addCSourceFiles(.{
        .root = upstream.path(""),
        .files = &.{
            "adler32.c",
            "crc32.c",
...
```

It is the `.name` argument to `addStaticLibrary` that tells us the name of the artifact. Zig packages might expose multiple such artifacts, as is the case for [mbedtls](https://github.com/actonlang/mbedtls/blob/zig-build/build.zig).

`acton zig-pkg add` will fetch the package from the provided URL and save the hash sum to `Build.act`, resulting in:
```python
dependencies = {}

zig_dependencies = {
  "zlib": (
        url="https://github.com/allyourcodebase/zlib/archive/refs/tags/1.3.1.tar.gz",
        hash="122034ab2a12adf8016ffa76e48b4be3245ffd305193edba4d83058adbcfa749c107",
        artifacts=["z"]
    ),
}
```

## Create zlib.act Acton module

Next up we need to create the Acton `zlib` module. Open `src/zlib.act` and add a compress and decompress function:

```python
pure def compress(data: bytes) -> bytes:
    NotImplemented

pure def decompress(data: bytes) -> bytes:
    NotImplemented
```

The `NotImplemented` statement tells the compiler that the implementation is not written in Acton but rather external. When there is a `.ext.c` file, the compiler expects it to contain the implementations for the `NotImplemented` functions. Also note the explicit types. Normally the Acton compiler can infer types, but since there is no Acton code here, only C code, there is nothing to infer from.

Now create `src/zlib.ext.c` which is where we will do the actual implementation of these functions. We need to add a `__ext_init__` function, which runs on module load by the Acton RTS, which must always exist. There is nothing to do in particular for zlib so let's just create an empty function, like so:

```c
void zlibQ___ext_init__() {}
```

Next, we need to fill in the C functions that map to the Acton functions `compress` and `decompress`. By invoking `acton build` we can get the compiler to generate a skeleton for these. We will also get a large error message, since there is no actual implementation:

```console
user@host$ acton build
... some large error message
```
Ignore the error and instead check the content of `out/types/zlib.c` and we will find the C functions we need, commented out:
```
#include "rts/common.h"
#include "out/types/zlib.h"
#include "src/zlib.ext.c"
B_bytes zlibQ_compress (B_bytes data);
/*
B_bytes zlibQ_compress (B_bytes data) {
    // NotImplemented
}
*/
B_bytes zlibQ_decompress (B_bytes data);
/*
B_bytes zlibQ_decompress (B_bytes data) {
    // NotImplemented
}
*/
int zlibQ_done$ = 0;
void zlibQ___init__ () {
    if (zlibQ_done$) return;
    zlibQ_done$ = 1;
    zlibQ___ext_init__ ();
}
```

Copy the commented-out skeleton into our own `src/zlib.ext.c`. Just in order to get something that compiles, let's just quickly let the functions return the input data. Since both input and output are `bytes`, this should now compile (and work at run time).
```c
B_bytes zlibQ_compress (B_bytes data) {
    return data;
}
B_bytes zlibQ_decompress (B_bytes data) {
    return data;
}
```

```console
user@host:~/acton-zlib$ acton build
Building project in /Users/user/acton-zlib
  Compiling zlib.act for release
   Finished compilation in   0.005 s
  Compiling test_zlib.act for release
   Finished compilation in   0.019 s
  Final compilation step
user@host:~/acton-zlib$
```

## Add a test module

Before we implement the body of the compress and decompress functions, we can write a small test module which will tell us when we've succeeded. We use some pre-known test data (which we could get from another language implementation):
```python
import testing
import zlib

def _test_roundtrip():
    for x in range(100):
        i = "hello".encode()
        c = zlib.compress(i)
        d = zlib.decompress(c)
        testing.assertEqual(i, d)

def _test_compress():
    for x in range(100):
        i = "hello".encode()
        c = zlib.compress(i)
        testing.assertEqual(c, b'x\x9c\xcbH\xcd\xc9\xc9\x07')

def _test_decompress():
    for x in range(1000):
        c = b'x\x9c\xcbH\xcd\xc9\xc9\x07'
        d = zlib.decompress(c)
        testing.assertEqual(d, b'hello')
```

Note how we run a few test iterations to get slightly better timing measurements for performance testing. Run the test with `acton test`:

```console
user@host:~/acton-zlib$ acton test

Tests - module test_zlib:
  decompress:            FAIL:  195 runs in 50.728ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: b'x\x9c\xcbH\xcd\xc9\xc9\x07' B: b'hello'
  compress:              FAIL:  197 runs in 50.886ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: b'hello' B: b'x\x9c\xcbH\xcd\xc9\xc9\x07'
  roundtrip:             OK:  226 runs in 50.890ms

2 out of 3 tests failed (26.354s)

user@host:~/acton-zlib$ 
```

As expected, the roundtrip test goes through, since we just return the input data while the compress and decompress tests fail.

## Implement the compress function

Now let's fill in the rest of the owl. Below is the body of the `zlibQ_compress` function. The bulk of this code is not particularly interesting to this guide as it has more to do with standard C usage of zlib, but a few things are worth noting.

```c
B_bytes zlibQ_compress(B_bytes data) {
    if (data->nbytes == 0) {
        return data;
    }

    // Prepare the zlib stream
    int ret;
    z_stream stream;
    memset(&stream, 0, sizeof(stream));
    ret = deflateInit(&stream, Z_DEFAULT_COMPRESSION);
    if (ret != Z_OK) {
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("Unable to compress data, init error: %d", ret)));
    }

    // Set the input data
    stream.avail_in = data->nbytes;
    stream.next_in = (Bytef*)data->str;

    // Allocate the output buffer using Acton's malloc
    size_t output_size = deflateBound(&stream, data->nbytes);
    Bytef* output_buffer = (Bytef*)acton_malloc_atomic(output_size);
    stream.avail_out = output_size;
    stream.next_out = output_buffer;

    // Perform the deflate operation
    ret = deflate(&stream, Z_FINISH);
    if (ret != Z_STREAM_END) {
        $RAISE((B_BaseException)$NEW(B_ValueError, $FORMAT("Unable to compress data, error: %d", ret)));
    }

    // Clean up
    deflateEnd(&stream);

    return actBytesFromCStringNoCopy(output_buffer);
}
```

Memory management is always top of mind when writing C, as it the case here. We can allocate memory via the Acton GC-heap malloc or just plain `malloc()` (the non-GC heap, to be explicit). Since `zlibQ_compress` is pure, we have no state leaking out of the function other than via its return value. All return values must be allocated on the Acton GC heap, so we know we must use `acton_malloc` for any value that we return. Any other local variables within the function can use classic malloc, as long as we make sure to explicitly free it up. For class or actor methods, any allocation for class or actor attributes must be performed using the Acton GC malloc, since there is no destructor or similar where a free can be inserted, so using classic malloc would be bound to leak. Also note that in this particular case, we know that the returned bytes value itself is not going to contain any pointers, so by using `acton_malloc_atomic` we can get a chunk of memory that will not be internally scanned by the GC, which saves a bit of time and thus improves GC performance. If we allocate structs that do carry pointers, they must use the normal `acton_malloc()`.

`actBytesFromCStringNoCopy(output_buffer)` takes the `buffer` (already allocated via `acton_malloc_atomic()`) and wraps it up as a boxed value of the type `B_bytes` that we return.

Also note how we convert Zlib errors to Acton exceptions where necessary.

Running the test, the `compress` test now passes while roundtrip has stopped working (since decompress is not implemented yet):

```console
user@host:~/acton-zlib$ acton test
Tests - module test_zlib:
  decompress:            FAIL:  158 runs in 50.175ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: b'x\x9c\xcbH\xcd\xc9\xc9\x07' B: b'hello'
  compress:              OK:  167 runs in 50.225ms
  roundtrip:             FAIL:  147 runs in 50.266ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: b'hello' B: b'x\x9c\xcbH\xcd\xc9\xc9\x07'

2 out of 3 tests failed (0.941s)

user@host:~/acton-zlib$ 
```

## Implement the decompress function

Much like the compress function, the decompress function mostly relates to how zlib itself and its interface works. We use the same wrappers and transform errors to exceptions.

```c
B_bytes zlibQ_decompress(B_bytes data) {
    if (data->nbytes == 0) {
        return data;
    }

    // Prepare the zlib stream
    int ret;
    z_stream stream;
    memset(&stream, 0, sizeof(stream));

    ret = inflateInit(&stream);
    if (ret != Z_OK) {
        $RAISE((B_BaseException)$NEW(B_ValueError, $FORMAT("Unable to decompress data, init error: %d", ret)));
    }

    // Set the input data
    stream.avail_in = data->nbytes;
    stream.next_in = (Bytef*)data->str;

    // Allocate the output buffer using Acton's malloc
    size_t output_size = 2 * data->nbytes; // Initial output buffer size
    Bytef* output_buffer = (Bytef*)acton_malloc_atomic(output_size);
    memset(output_buffer, 0, output_size);
    stream.avail_out = output_size;
    stream.next_out = output_buffer;

    // Perform the inflate operation, increasing the output buffer size if needed
    do {
        ret = inflate(&stream, Z_NO_FLUSH);
        if (ret == Z_BUF_ERROR) {
            // Increase the output buffer size and continue decompressing
            size_t new_output_size = output_size * 2;
            output_buffer = (Bytef*)acton_realloc(output_buffer, new_output_size);
            stream.avail_out = new_output_size - stream.total_out;
            stream.next_out = output_buffer + stream.total_out;
        } else if (ret != Z_OK) {
            $RAISE((B_BaseException)$NEW(B_ValueError, $FORMAT("Unable to decompress data, error: %d", ret)));
        }
    } while (ret == Z_BUF_ERROR);

    // Clean up
    inflateEnd(&stream);

    return actBytesFromCStringNoCopy(output_buffer);
}
```

## Final test

```console
user@host:~/acton-zlib$ acton test

Tests - module test_zlib:
  decompress:            OK:   42 runs in 51.065ms
  compress:              OK:   25 runs in 50.032ms
  roundtrip:             OK:   24 runs in 50.053ms

All 3 tests passed (0.738s)

user@host:~/acton-zlib$ 
```

And with that, we're done! A simple wrapper around zlib, which is also available [on GitHub](https://github.com/actonlang/acton-zlib) if you want to study it further.
