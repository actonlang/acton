# Codegen

Implementation lives in `compiler/lib/src/Acton/CodeGen.hs`.

`Acton.CodeGen.generate` receives the hash string that should be stamped into
the generated output. It writes that hash as the first line of both `.c` and
`.h` files:

```c
/* Acton impl hash: ... */
```

For normal modules, `Acton.Compile` passes the module implementation hash. For
DBP modules, it passes the DBP codegen hash derived from the module
implementation hash plus the selected top-level names. The label is therefore
historical: in DBP output it is a selection-sensitive generated-code hash, not
just the raw module implementation hash.
