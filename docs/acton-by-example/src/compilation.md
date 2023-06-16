# Compilation

Acton is a compiled language and as such, outputs binary executables. It is possible to influence the compilation process in various ways.

## Optimized for native CPU features

The default target is somewhat conservative to ensure a reasonable amount of compatibility. On Linux, the default target is GNU Libc version 2.27 which makes it possible to run Acton programs on Ubuntu 18.04 and similar old operating systems. Similarly, a generic x86_64 CPU is assumed which means that newer extra CPU instruction sets are not used.

To compile an executable optimized for the local computer, use `--target native`. In many cases it can lead to a significant faster program, often running 30% to 100% faster.

## Cross-compilation

Acton supports cross-compilation, which means that it is possible to run develop on one computer, say a Linux computer with an x86-64 CPU but build an executable binary that can run on a MacOS computer.

Here's such an example. We can see how per default, the output is an ELF binary for x86-64. By setting the `--target` argument, `actonc` will instead produce an executable for a Mac.
```
user@machine$ actonc --quiet helloworld.act
user@machine$ file helloworld
helloworld: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.0.0, with debug_info, not stripped
user@machine$ actonc --quiet helloworld.act --target x86_64-macos-none
user@machine$ file helloworld
helloworld: Mach-O 64-bit x86_64 executable, flags:<NOUNDEFS|DYLDLINK|TWOLEVEL|PIE>
```

It is not only possible to compile for other operating systems, but also for other CPU architectures. For example, use `--target aarch64-macos-any` to produce a binary executable for an Apple M1/M2 CPU.

## Prebuilt libraries

Acton ships with prebuilt libraries for the local platforms default target, i.e. if you install Acton on a x86-64 Linux machine, it will have libraries prebuilt for x86_64-linux-gnu.2.27. The default target uses these prebuilt libraries which results in a fast build:
```
user@machine:~$ actonc helloworld.act
Building file helloworld.act
  Compiling helloworld.act for release
   Finished compilation in   0.013 s
  Final compilation step
   Finished final compilation step in   0.224 s
user@machine:~$
```

When targeting something that is not the default target, the entire Acton system, including builtins, the run time system, the standard library and external library dependencies is built from source and can take a significant amount of time. The build process is highly parallelized and cached. For example, on an AMD 5950X with 16 cores / 32 threads, it takes around 7 seconds to do a complete rebuild for a small Acton program as can be seen here:
```
user@machine:~$ actonc helloworld.act --target aarch64-macos-none
Building file helloworld.act
  Compiling helloworld.act for release
   Finished compilation in   0.012 s
  Final compilation step
   Finished final compilation step in   6.847 s
user@machine:~$
```

## Build cache

In an Acton project, there is a build cache, is is stored in a directory called `build-cache` in the project directory. The cache is always used for the project local files. If a non-default `--target` is being used, the built output of the Acton system is also stored in the cache, which means that it is only the first time around that it is slow. Any subsequent build is going to use the cache and run very fast. Like in this example, where the first invocation takes 6.120 seconds and the second one runs in 0.068 seconds.

```
user@machine:~$ actonc new hello
Created project hello
Enter your new project directory with:
  cd hello
Compile:
  actonc build
Run:
  ./out/rel/bin/hello

Initialized empty Git repository in /home/kll/hello/.git/
user@machine:~$ cd hello/
user@machine:~/hello$ actonc build --target native
Building project in /home/kll/hello
  Compiling hello.act for release
   Finished compilation in   0.012 s
  Final compilation step
   Finished final compilation step in   6.120 s
user@machine:~/hello$ actonc build --target native
Building project in /home/kll/hello
  Compiling hello.act for release
   Already up to date, in    0.000 s
  Final compilation step
   Finished final compilation step in   0.068 s
user@machine:~/hello$
```

When compiling standalone .act files, there is no project and thus no persistent cache, so using a custom `--target` will always incur a penalty.
