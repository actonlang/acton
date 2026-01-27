Acton LLDB Plugin (Prototype)

Python plugin to improve debugging of Acton programs in LLDB on macOS. Provides filtered/demangled backtraces, readable locals, and Acton‑aware pretty printing.

Commands
- `acton bt` — Filtered backtrace with Acton demangling and argument values
  - Flags:
    - `-a, --all` include hidden frames (RTS, system)
    - `-f, --full` include all threads
    - `--no-demangle` turn off demangling
    - `--show-original` show original symbol next to demangled
    - `--hide-regex REGEX` add extra hide regex (repeatable)
    - `--clear-hides` clear previously added hide regexes
    - `--color/--no-color` toggle ANSI colors for file:line
    - `--no-arg-values` do not print argument values in bt
- `acton locals` — In‑scope args/locals with demangled names and value summaries
  - Flags:
    - `--no-demangle`
    - `--all` include statics, out‑of‑scope and artificial
    - `--args-only` only show function arguments
    - `--no-hide-meta` include meta/runtime variables (by default hides `$`/`__` vars, method tables, `$class`, and `C_cont`)
- `acton demangle [name]` — Demangle a single name; if omitted, uses the selected frame function
- `acton break <file.act>:<line>` — Set a breakpoint by Acton source location. Alias: `acton-bp`

Aliases
- Hyphenated: `acton-bt`, `acton-locals`, `acton-demangle`
- Dotted (if your LLDB accepts it): `acton.bt`, `acton.locals`, `acton.demangle`

Loading
1) One‑off inside LLDB:
   - `(lldb) command script import /absolute/path/to/acton/utils/lldb/acton.py`
   - Reload after edits with `-r`:
     - `(lldb) command script import -r /absolute/path/to/acton/utils/lldb/acton.py`
2) Auto‑load on startup (`~/.lldbinit`):
   - `command script import /absolute/path/to/acton/utils/lldb/acton.py`

Source breakpoints (Acton file:line)
- Requires debug info that references `.act` files (build with `acton --debug`).
- Examples:
  - `(lldb) breakpoint set -f stackdemo.act -l 7`
  - `(lldb) acton break stackdemo.act:7`
- If no locations resolve, the helper prints source‑map hints based on compile units. You can map build paths to your checkout:
  - `(lldb) settings set target.source-map /old/build/root /Users/me/src/acton`

Demangling and name rewrites
- Heuristics to improve readability:
  - `Q_` → `.` (module separators)
  - `D___` → `.` and `D_` → `.` (method separators)
  - `G_` → `.` (generic/section)
  - `$` → `.` (C identifiers like `to$int` → `to.int`)
  - Drop `B_` prefix for builtins (e.g. `B_str` → `str`)
  - Drop `U_` prefix for unboxed C locals and trim leading numeric index: `U_5v3` → `v3`
  - Strip trailing `.local` synthesized from `G_local`

Value summaries / pretty printing
- Backtrace arguments and locals:
  - Boxed values are stringified via `B_value.__str__` and printed. This covers objects/actors/classes (e.g. `<stackdemo.main object at 0x…>`), builtin numbers, etc.
  - Strings (`B_str`) are read directly and printed quoted (UTF‑8, with escaping).
  - Unboxed C types (e.g. `uint64_t`, `int64_t`) use LLDB’s native formatting.
  - The CPS continuation arg `C_cont` is hidden by default in `acton locals` and excluded from bt arg lists.
  - If stringification fails, we fall back to numeric conversions for simple builtins, then raw pointer.

Colors
- By default, `acton bt` prints file:line with colors (file in cyan, line in yellow). Use `--no-color` or set `ACTON_LLDB_NO_COLOR=1` to disable.

Environment overrides
- `ACTON_LLDB_HIDE_REGEX` — comma‑separated regexes to hide symbol names
- `ACTON_LLDB_NO_DEMANGLE` — disable demangling
- `ACTON_LLDB_SHOW_ORIGINAL` — print original symbol next to demangled
- `ACTON_LLDB_NO_COLOR` / `ACTON_LLDB_COLOR` — control colors
- `ACTON_LLDB_NO_ARG_VALUES` — disable argument value printing in bt

Batch usage examples
- Quick one‑liner:
  - `lldb examples/stackdemo -o 'command script import utils/lldb/acton.py' -o 'b stackdemo.act:7' -o 'run' -o 'acton locals' -o 'acton bt' --batch`
- With module‑wide breakpoints:
  - `lldb -o 'command script import utils/lldb/acton.py' -o 'br s -r "stackdemoQ_.*"' -o run -- examples/stackdemo`

Troubleshooting
- Ensure your binary has debug info pointing at `.act` files (compile with `acton --debug`).
- If bt/locals do not show Acton files, check source‑map settings.
- If value printing is slow or too verbose in bt, pass `--no-arg-values`.

Notes
- This is a prototype; the demangler is heuristic. We can refine as needed and later move these capabilities to a native C++ plugin for deeper integration.
