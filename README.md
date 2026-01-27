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
Type inference means you don't have to explicitly declare types of every
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


# Getting started with Acton

## Install Acton

Check out the [installation guide](https://www.acton-lang.org/install) for more details.

### Debian / Ubuntu
```sh
sudo install -m 0755 -d /etc/apt/keyrings
sudo wget -q -O /etc/apt/keyrings/acton.asc https://apt.acton-lang.io/acton.gpg
sudo chmod a+r /etc/apt/keyrings/acton.asc
echo "deb [signed-by=/etc/apt/keyrings/acton.asc arch=amd64] http://apt.acton-lang.io/ stable main" | sudo tee -a /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
```

### Mac OS X
```sh
brew install actonlang/acton/acton
```

## Writing and compiling your first Acton program

Edit the program source file, let's call it `helloworld.act`, and enter the
following code:

``` Acton
actor main(env):
    print("Hello, world!")
    env.exit(0)
```

Compile the program and run it:

```
$ acton helloworld.act
$ ./helloworld
Hello, world!
```

## And then...?
Check out our [learning resources](https://www.acton-lang.org/learn).


# Building Acton from source
See [building Acton from source](https://www.acton-lang.org/install/from-source).

# Developing Acton
See [dev info](docs/dev.md).

# Contributions

For information about contributing to Acton, see [CONTRIBUTING.md](CONTRIBUTING.md).

## Thanks to all the people who already contributed!

<a href="https://github.com/actonlang/acton/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=actonlang/acton" />
</a>
