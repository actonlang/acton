# The Acton programming language
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

# Building

## Build dependencies

- stack
- git
- make (GNU)
- gcc

### Debian
Install prerequisites
```
apt install alex gcc happy haskell-stack libutf8proc-dev make
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
