# Zig issue draft — file at https://github.com/ziglang/zig/issues/new

**Title:** `zig cc`: glibc stub gives aliased data symbols (`environ`, `tzname`, …) distinct addresses, so copy relocations read stale/NULL values

---

### Zig Version

`0.16.0` (bundled lld / clang 21.1.0)

### Summary

Zig's synthesized glibc stub does not preserve glibc's **data-symbol aliasing**. Variables that real glibc exports as several weak aliases of one object at one address (e.g. `environ`/`_environ`/`__environ`) are emitted by the stub as **separate `STB_GLOBAL` symbols at distinct addresses**.

When `zig cc` links a `-no-pie` executable from a **non-PIC** object that references such a symbol, lld emits an independent `R_X86_64_COPY` per name. glibc then initializes the value at runtime through its internal `__`-alias (e.g. `__libc_start_main` writes `__environ`), but the executable reads its *own* copy of the public name (`environ`), which is never populated → it reads NULL/stale.

This is not specific to `environ` — it affects the whole class of aliased glibc data variables that glibc writes at runtime through a `__`-internal name (see *Scope*). It is also distinct from #8896 (closed; that fixed `STT_FUNC` vs `STT_OBJECT` typing and symbol sizes): data symbols now have the right type/size, but their **aliasing** is still lost.

### Steps to Reproduce

`zig cc` always compiles C as PIC (its own objects use GOT access and are unaffected), so the non-PIC object below is produced with another toolchain — which is exactly the real-world trigger (e.g. a GHC bindist; see *Real-world impact*).

```c
// env_repro.c
#include <stdio.h>
#include <stdlib.h>
extern char **environ;
int main(void) {
    printf("getenv(\"HOME\") = %s\n", getenv("HOME"));
    printf("environ        = %p\n", (void *)environ);
    printf("environ[0]     = %s\n", environ && environ[0] ? environ[0] : "(empty / NULL)");
    return 0;
}
```

```console
$ gcc -fno-pic -no-pie -c env_repro.c -o env_repro.o        # any non-PIC object
$ zig cc -target x86_64-linux-gnu.2.31 -no-pie env_repro.o -o env_repro
$ env -i HOME=/tmp/x FOO=bar ./env_repro
getenv("HOME") = /tmp/x
environ        = (nil)            # ← BUG: should be a valid pointer
environ[0]     = (empty / NULL)
```

Linking the identical source with the system toolchain (real glibc) works:

```console
$ gcc -no-pie env_repro.c -o env_repro_gcc
$ env -i HOME=/tmp/x FOO=bar ./env_repro_gcc
getenv("HOME") = /tmp/x
environ        = 0x7ffc...        # ← correct
environ[0]     = HOME=/tmp/x
```

### Scope — not just `environ`

The same `zig cc -no-pie` vs system-`gcc` comparison, for several aliased glibc globals that glibc sets at runtime (one program: read them after `tzset()`):

| variable | written by glibc via | `zig cc` link | `gcc` link (correct) |
|---|---|---|---|
| `environ` | `__environ` (`__libc_start_main`) | `(nil)` | valid (`HOME=/tmp/x`) |
| `tzname[0]` | `__tzname` (`tzset`) | `GMT` (stale) | `EST` |
| `timezone` | `__timezone` (`tzset`) | `0` | `18000` |
| `daylight` | `__daylight` (`tzset`) | `0` | `1` |
| `program_invocation_name` | `__progname` (startup) | *(empty)* | the path |

All four get `R_X86_64_COPY` in the zig-linked binary.

<details><summary>multi-symbol reproduction source</summary>

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
extern char **environ;
extern char *program_invocation_name;
int main(void) {
    tzset();
    printf("environ                 = %p (%s)\n", (void*)environ, environ&&environ[0]?environ[0]:"NULL");
    printf("tzname[0]               = %s\n", tzname[0]?tzname[0]:"(NULL)");
    printf("timezone                = %ld\n", timezone);
    printf("daylight                = %d\n", daylight);
    printf("program_invocation_name = %s\n", program_invocation_name?program_invocation_name:"(NULL)");
    return 0;
}
// gcc -fno-pic -no-pie -c multi.c -o multi.o
// zig cc -target x86_64-linux-gnu.2.31 -no-pie multi.o -o multi_zig
// env -i HOME=/x TZ=America/New_York ./multi_zig
```
</details>

**Trigger condition** (all three needed): a copy relocation for the public symbol (non-PIC reference + `-no-pie`); glibc updates the value at runtime via a *different* (`__`) name; and the stub places those names at different addresses.

**Not affected:** functions (resolved via PLT, not copy relocations; `errno`/`h_errno` are the function `__errno_location()` + TLS); and data that is single-named or initialized at load time — `stdin`/`stdout`/`stderr` (set before copy-relocs are applied) and `optind` (read and written under the same name) come out correct.

### Root cause

The reference from the non-PIC object is absolute (`R_X86_64_64`), so lld emits a copy relocation, and the aliases are not unified:

```console
$ readelf -r env_repro | grep environ
R_X86_64_COPY  environ@GLIBC_2.2.5
```

Stub vs real glibc — the stub does not co-locate the aliases and marks them GLOBAL instead of WEAK:

```
# zig stub (target x86_64-linux-gnu.2.31)
   environ@@GLIBC_2.2.5   0x244b0   OBJECT GLOBAL  size 8
  _environ@@GLIBC_2.2.5   0x244c8   OBJECT GLOBAL  size 8     # different address, GLOBAL
    tzname@@GLIBC_2.2.5   0x23ed8   OBJECT GLOBAL  size 16
  __tzname@@GLIBC_2.2.5   0x23ef0   OBJECT GLOBAL  size 16    # different address from tzname

# real /lib/x86_64-linux-gnu/libc.so.6
   environ@@GLIBC_2.2.5   0x20ad58  OBJECT WEAK    size 8
  _environ@@GLIBC_2.2.5   0x20ad58  OBJECT WEAK    size 8     # same address, WEAK (aliased)
```

In `src/libs/glibc.zig`, each symbol is emitted with its own storage and global binding, with no alias awareness:

```asm
.globl environ_2_2_5
.type  environ_2_2_5, %object
.size  environ_2_2_5, 8
environ_2_2_5: .fill 8, 1, 0          # separate .fill ⇒ distinct address
```

and each `abilists` entry carries only `(name, targets, size, lib, versions)` — no alias-group information — so neither layer knows these names share one object.

### Suggested fix

Preserve glibc's aliasing. Two parts:

1. **Record aliases (data layer).** The abilists generator ([ziglang/libc-abi-tools](https://github.com/ziglang/libc-abi-tools), which reads real glibc symbol tables) should group data symbols that share an `st_value`, and the binary `abilists` format should encode those groups.
2. **Emit them co-located (`src/libs/glibc.zig`).** For an alias group, emit one storage and alias the other names to the same address, with the public names weak — mirroring glibc:

```asm
.globl __environ_2_2_5
.type  __environ_2_2_5, %object
.size  __environ_2_2_5, 8
__environ_2_2_5: .fill 8, 1, 0
.weak  environ_2_2_5
.set   environ_2_2_5,  __environ_2_2_5     # alias ⇒ same address
.weak  _environ_2_2_5
.set   _environ_2_2_5, __environ_2_2_5
```

Co-location is the essential part: a single copy relocation then covers `__environ` too, so `__libc_start_main`'s write lands in the executable's copy. The `.weak` binding matches glibc so a program can still define its own `environ`.

A cheaper stopgap, if extending the abilists format is undesirable, is a hardcoded alias table in `glibc.zig` for the known runtime-written globals (`environ`/`_environ`/`__environ`, `tzname`/`__tzname`, `timezone`/`__timezone`, `daylight`/`__daylight`, `program_invocation_name`/`__progname`, `program_invocation_short_name`/`__progname_full`).

### Expected Behavior

A copy-relocated `environ` (and the other aliased globals) should read the live value, matching a link against real glibc.

### Real-world impact

GHC reads the environment via `environ` (`__hscore_environ()` returns `environ`, in both GHC 9.8 and `master`). Any GHC program linked with `zig cc -no-pie` therefore gets `System.Environment.getEnvironment == []`. Concretely, the Acton compiler (Haskell, linked through `zig cc` for older-glibc portability) silently ignored `HTTP_PROXY`/`HTTPS_PROXY` — `http-client`'s `proxyEnvironment` reads `getEnvironment`, so package fetching failed behind a corporate proxy.
