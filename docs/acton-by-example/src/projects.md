# Acton Projects

Besides compiling individual `.act` files, it is possible to organize Acton source files into a an **Acton Project**, which simplifies many common tasks.

Use `actonc` to create a project called `foo`:
```sh
actonc new foo
```

Output:
```sh
Created project foo
Enter your new project directory with:
  cd foo
Compile:
  actonc build --root foo.main
Run:
  ./out/rel/bin/foo
```

## Description

Use `actonc build` to build a project. The current working directory must be the project directory or a sub-directory to the project directory. `actonc` will discover all source files and compile them according to dependency order.

Specify a root actor with a qualified name, like `foo.main`, to produce an executable.
