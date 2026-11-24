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
- `acton refs [-s|--summary] [-d|--depth N] ADDRESS...` — What keeps GC objects alive (see below)
- `acton jump0` — The GC objects that each worker's `jump0` exception frame points to
- `acton outgoing ACTOR...` — The mailbox head and the `$outgoing` messages of actors

Aliases
- Hyphenated: `acton-bt`, `acton-locals`, `acton-demangle`, `acton-refs`, `acton-jump0`, `acton-outgoing`
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

What keeps a GC object alive
- `acton refs`, `acton jump0` and `acton outgoing` read a stopped program, at a breakpoint or attached with `lldb -p PID`. They take struct layouts from the debug info of the RTS and the collector. To find addresses, print the objects: an actor prints as `<$Actor -1089 keepaliveQ_Leaf at 0x107b03e20>`.
- `acton refs ADDRESS...` searches what the collector scans for pointers (its heap, its static roots, each thread's stack from the stack pointer up, and the registers) for words that point into the GC object at ADDRESS. Then it does the same for each marked GC object among them, until each branch ends at a root. ADDRESS is a number or an expression, such as `self`.
  - Each `<-` line is something that points into the object above it: a GC object (its start and the offset of the pointer in it, the symbol that its first word points to, which is the class of an Acton object, its size, mark bit and kind), a static root, a stack frame (with the local variable that holds the pointer), or a register.
  - The mark bits are those of the last collection. The walk does not go past unmarked objects, which were garbage then, nor past pointer-free objects, whose words the collector does not scan. Uncollectable objects are roots. Run a collection right before you stop the program, for example with `env.rts.gc()`.
  - `-s/--summary` prints one line per root instead of the tree, with the number of words in the root that lead to ADDRESS and the objects in between, for example `42x ?(272) <- stack of thread #11 (Worker 1), uv__io_poll`. `-d/--depth N` limits the walk to N objects (default 10).
  - Example: a `Leaf` actor with `__cleanup__` that a `Holder` actor keeps in a list:
    ```
    (lldb) acton refs 0x107b03e20
    searched 10 MiB of heap, 2 MiB of static roots, and the stacks (3 MiB) and registers of 24 threads
    0x107b03e20: keepaliveQ_LeafG_methods, 112 bytes, marked
      <- 0x107b07100+0: ?, 32 bytes, marked
        <- 0x107b07fc0+8: B_listG_methods, 32 bytes, marked
          <- 0x107b03f70+96: keepaliveQ_HolderG_methods, 112 bytes, marked
            <- 0x107a3fd80+104: keepaliveQ_mainG_methods, 128 bytes, marked
              <- static keepalive`root_actor+0
              <- 0x107a5ff20+16: B_MsgG_methods, 80 bytes, unmarked
    ```
- `acton jump0` prints, for every worker, `jump_top` and each word of the `jump0` exception frame in its `WorkerCtx` that points into a GC object. The first continuation in each call of `wt_work_cb()` pushes `jump0` with a setjmp, which saves the worker's registers in it, and the collector scans them until `clear_before_poll()` drops the frame before the worker polls. Example, in a `--fast` build stopped in a continuation on worker 3:
  ```
  (lldb) acton jump0
  worker 0: jump_top NULL, 0 of 26 words of jump0 are not 0
  worker 3: jump_top jump0, 12 of 26 words of jump0 are not 0
      buf: 0x104528000, ?
      buf+32: 0x10457ffa0, keepaliveQ_L_6procG_methods
  ```
- `acton outgoing ACTOR...` prints the head of each actor's mailbox and the messages in its `$outgoing` list, newest first, with the recipient of each. A continuation's messages wait there until the worker delivers them after the continuation. Example, at a breakpoint after `Sender.go` sent two messages:
  ```
  (lldb) acton outgoing self
  0x10524fe90 keepaliveQ_SenderG_methods: mailbox 0x105287f70, $outgoing 0x105287ed0
      message 0x105287ed0 to 0x10524ff00 keepaliveQ_LeafG_methods
      message 0x105287f20 to 0x10524ff00 keepaliveQ_LeafG_methods
  ```

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
