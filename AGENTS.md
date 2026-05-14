Acton compiler notes:

- `acton build --no-threads` controls whether the produced Acton binary uses
  threads in Acton RTS. It does not disable threads in the Acton compiler
  process itself, so compilation still happens concurrently. This flag is only
  to be used for compatibility with platforms that do no have thread support,
  for example cross-compiling for Windows, we build without threads.
