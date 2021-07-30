# The Acton programming language
[![Test](https://github.com/actonlang/acton/actions/workflows/test.yml/badge.svg)](https://github.com/actonlang/acton/actions/workflows/test.yml)

Acton is a general purpose programming language, designed to be useful for a
wide range of applications, from desktop applications to embedded and
distributed systems. In a first approximation Acton can be described as a
seamless addition of a powerful new construct to an existing language: Acton
adds *actors* to *Python*.

Acton is a compiled language, offering the speed of C but with a considerably
simpler programming model. There is no explicit memory management, instead
relying on garbage collection.

Acton is statically typed with an expressive type language and type inference.
Type inferrence means you don't have to explicitly declare types of every
variable but that the compiler will *infer* the type and performs its checks
accordingly. We can have the benefits of type safety without the extra overhead
involved in declaring types.

The Acton Run Time System (RTS) offers a distributed mode of operation allowing
multiple computers to participate in running one logical Acton system. Actors
can migrate between compute nodes for load sharing purposes and similar. The RTS
offers exactly once delivery guarantees. Through checkpointing of actor states
to a distributed database, the failure of individual compute nodes can be
recovered by restoring actor state.

NOTE: Acton is in an experimental phase and although much of the syntax has been
worked out, there may be changes.

NOTE: The RTS currently does not have a garbage collector, severely limiting it
for long running tasks. However, for smaller shorter lived processes, it can
work fairly well.


# Building

## Build dependencies

- stack
- git
- make (GNU)
- gcc

### Debian
Install prerequisites
```
apt install alex gcc happy haskell-stack libkqueue-dev libprotobuf-c-dev libutf8proc-dev make
```

### Mac OS X

```
brew install argp-standalone haskell-stack protobuf-c util-linux
```


## Build instructions

To build the project, simply run:

```
make
```

## Running tests
```
make -C test
```
