/*
 * environ_fix.c -- repair getEnvironment() on Linux binaries linked by `zig cc`.
 *
 * The compiler is linked as a -no-pie executable by zig cc (to target an older
 * glibc for portability). Under that link lld emits a copy relocation for the
 * glibc `environ` symbol but does not redirect glibc's `__environ` alias to the
 * copied storage. __libc_start_main() initialises `__environ` (so getenv() and
 * base's lookupEnv keep working), while the executable's copied `environ` -- the
 * one GHC's RTS reads for System.Environment.getEnvironment -- stays NULL. So
 * getEnvironment() returns [], and e.g. http-client's proxyEnvironment never
 * sees HTTP(S)_PROXY.
 *
 * Defining `environ` here (a strong definition in the executable) means no copy
 * relocation is generated for it, and a startup constructor points it at glibc's
 * live `__environ`, so getEnvironment() sees the real environment.
 *
 * The snapshot is taken at startup; the compiler never calls setenv()/putenv()
 * (which could reallocate the block), so it stays valid, and getenv()/lookupEnv
 * read glibc's live `__environ` regardless.
 *
 * Linux/glibc only -- gated to os(linux) in package.yaml.in (macOS builds are
 * PIE and expose no `__environ`).
 */
extern char **__environ;
char **environ;

__attribute__((constructor))
static void acton_fix_environ(void) {
    environ = __environ;
}
