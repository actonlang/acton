# C / C++ / Zig dependencies

Much like dependencies on other Acton packages, an Acton project can depend on a Zig package which could be a C / C++ or Zig library, as long as it has a `build.zig` file.

- `acton zig-pkg add URL NAME --artifact X --artifact`
  - list the libraries you want to link with as artifacts
- `acton zig-pkg remove NAME`
