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
recovered by restoring actor state. Your system can run forever!

NOTE: Acton is in an experimental phase and although much of the syntax has been
worked out, there may be changes.

NOTE: The RTS currently does not have a garbage collector, severely limiting it
for long running tasks. However, for smaller shorter lived processes, it can
work fairly well.


# Getting started with Acton

## Install acton

### Debian / Ubuntu via apt repository

Install Acton via apt repository:

```sh
wget -q -O - https://apt.acton-lang.io/acton.gpg | sudo apt-key add -
echo "deb http://apt.acton-lang.io/ bullseye main" | sudo tee /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
```

### Mac OS X using Homebrew

Acton is available as a Homebrew tap, which can be installed with:
```
brew install actonlang/acton/acton
```

### By downloading a binary release tar ball

There are pre-built binary release tar balls available for download for Linux
and Mac OS X in case the above package formats are not suitable. See the guide,
[installing Acton from a release tar ball](docs/install-acton-from-tar-ball.md).


## Writing and compiling your first Acton program

Edit the program source file, let's call it `helloworld.act`, and enter the
following code:

``` Acton
actor main(env):
    print("Hello, world!")
    await async env.exit(0)
```

Compile the program and run it:

```
$ actonc --root main helloworld.act
$ ./helloworld
Hello, world!
```

## And then...?
Go read the [tutorial!](docs/tutorial/index.html)


# Building Acton from source
See [building Acton from source](docs/building-acton-from-source.md).
