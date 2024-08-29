# Acton Projects

Besides compiling individual `.act` files, it is possible to organize Acton code into an **Acton Project**, which is suitable once you have more than one `.act` source code file.

Use `acton` to create a new project called `foo`:
```console
acton new foo
```

Output:
```console
Created project foo
Enter your new project directory with:
  cd foo
Compile:
  acton build
Run:
  ./out/bin/foo
```

## Description

Use `acton build` to build a project. The current working directory must be the project directory or a sub-directory to the project directory. `acton` will discover all source files and compile them according to dependency order.

Add a `main` actor to any source file directly under `src/` to produce an executable binary. For example, if `src/hello.act` contains a `main` actor, it will produce `out/bin/hello` using `main` as the root actor.
