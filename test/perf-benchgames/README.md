# Benchmark programs

These are the Acton programs of the Programming Language Benchmarks suite in
https://github.com/actonlang/Programming-Language-Benchmarks (PLB). Each
directory has the layout of a PLB `bench/algorithm/<problem>` directory:

- Each `.act` file is a program for the problem, for example `1.act` and
  `2.act`. The Acton programs are maintained here. Before each run, PLB's Acton
  jobs replace the `.act` files in `bench/algorithm/<problem>` with the files
  here at the `tip` tag. PLB builds the programs that `bench/bench_acton.yaml`
  and `bench/bench_acton_tip.yaml` list for the problem. The latest release and
  the tip build both compile these files, so a program that needs a newer
  compiler than the latest release leaves PLB without stable Acton results.
- `<input>_out` is the expected output of one of PLB's unit tests for the
  problem. PLB's `bench/bench.yaml` lists each unit test's input and output
  file.
- `LICENSE` covers programs derived from the Computer Language Benchmarks Game.
- Other files are input data that the program reads from its working
  directory.

## Checking the programs

`make test-benchgames` builds every program and compares its output with the
`_out` files. As in PLB, trailing whitespace does not count. It runs the
"Benchmark programs" group of the compiler test suite, which `make test` leaves
out and CI runs as a separate step. The input of knucleotide and regex-redux is
the output of fasta: `fasta 25000` writes PLB's `25000_in`.

## Running a program

Build an optimized program and run it with an input from the `tests` list in
PLB's `bench/bench.yaml`:

```sh
dist/bin/acton --release test/perf-benchgames/pidigits/1.act
test/perf-benchgames/pidigits/1 4000
```
