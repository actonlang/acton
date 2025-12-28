# Parse

Implementation lives in `compiler/lib/src/Acton/Parser.hs`.

- Parses `.act` source into the core AST (`Module`).
- Uses the Haskell Megaparsec library
- Tracks parse context (top-level, actor, loop, etc.) to enforce where
  statements are allowed.
- Attaches source locations so later stages can produce precise errors.

The entry point is `parseModule`, which takes a module name, filename, and file
contents, then returns the parsed `Module`. It also appends a trailing newline
if the file is missing one to keep the parser consistent.

## Debugging

Use compiler flags to inspect the parser output:

```sh
actonc --parse path/to/file.act
actonc --parse-ast path/to/file.act
```

## Tests

There are unit tests for the parser in ActonSpec.hs, in particular the string interpolation support is fairly well tested. Please add more tests.

## Thoughts
- It is hard to write a parser. It is even harder to write a parser with good error reporting... and once you do, it's probably not going to perform very well.
- Our parser has some good error reporting, in particular around string interpolation. It can be improved in other areas. The aspiration as always is super friendly error messages.
- TODO: When we encounter errors, the parser should be able to recover and continue parsing the rest of the file. We'd need to mark such nodes as Bad nodes (I think the Bad name is a good one) and also accumulate a list of errors instead of immediately reporting errors and exiting
