const std = @import("std");

// Builds a static libgmp.a from the upstream GMP source tarball, pure C only
// (assembly disabled), so the same build works for both x86_64 and aarch64
// linux targets and we control the target glibc version. Adapted from
// Rexicon226/zig-gmp, upgraded to zig 0.16 and stripped of asm / C++ (gmpxx).
pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const src = b.dependency("gmp_src", .{});

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "gmp",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    const mod = lib.root_module;
    mod.link_libc = true;

    const limb_bits: i64 = switch (target.result.cpu.arch) {
        .x86_64, .aarch64 => 64,
        else => |t| std.debug.panic("unsupported gmp target arch: {s}", .{@tagName(t)}),
    };

    // Public gmp.h, generated from gmp-h.in (@VAR@ / cmake style).
    const gmp_header = b.addConfigHeader(.{
        .style = .{ .cmake = src.path("gmp-h.in") },
        .include_path = "gmp.h",
    }, .{
        .HAVE_HOST_CPU_FAMILY_power = false,
        .HAVE_HOST_CPU_FAMILY_powerpc = false,
        .GMP_LIMB_BITS = limb_bits,
        .GMP_NAIL_BITS = 0,
        .DEFN_LONG_LONG_LIMB = .@"#define _LONG_LONG_LIMB 1",
        .LIBGMP_DLL = 0,
        .CC = "zigcc",
        .CFLAGS = "",
    });
    mod.addConfigHeader(gmp_header);
    lib.installConfigHeader(gmp_header);

    // Internal config.h, generated from config.in (#undef / autoconf style).
    mod.addConfigHeader(makeConfigHeader(b, src, target));

    // gmp-impl.h pulls in gmp.h, config.h (both from the ConfigHeader output
    // dirs, added automatically), gmp-mparam.h (per-arch) and the generated
    // tables below.
    mod.addIncludePath(src.path("."));
    mod.addIncludePath(src.path(switch (target.result.cpu.arch) {
        .x86_64 => "mpn/x86_64",
        .aarch64 => "mpn/arm64",
        else => unreachable,
    }));

    // Generated lookup tables. Each gen-*.c is built and run for the host, its
    // stdout captured into a header placed in a shared WriteFiles dir that is
    // then on the include path.
    const gen = b.addWriteFiles();
    mod.addIncludePath(gen.getDirectory());
    _ = gen.addCopyFile(try genTable(b, src, "gen-fib", "header", 0, limb_bits), "fib_table.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-fac", null, 0, limb_bits), "fac_table.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-sieve", null, null, limb_bits), "sieve_table.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-bases", "header", 0, limb_bits), "mp_bases.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-jacobitab", null, null, null), "jacobitab.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-psqr", null, 0, limb_bits), "perfsqr.h");
    _ = gen.addCopyFile(try genTable(b, src, "gen-trialdivtab", null, 8000, limb_bits), "trialdivtab.h");

    mod.addCSourceFiles(.{
        .root = src.path("."),
        .files = &top_level_files,
        .flags = &.{"-DCOUNT_LEADING_ZEROS_NEED_CLZ_TAB"},
    });
    // mp_bases.c is also generated (table form of gen-bases).
    mod.addCSourceFile(.{
        .file = gen.addCopyFile(try genTable(b, src, "gen-bases", "table", 0, limb_bits), "mp_bases.c"),
    });
    // fib_table.c (table form of gen-fib) defines __gmp_fib_table[], used by
    // mpn fib2_ui; the header form above only emits the FIB_TABLE_LIMIT macros.
    mod.addCSourceFile(.{
        .file = gen.addCopyFile(try genTable(b, src, "gen-fib", "table", 0, limb_bits), "fib_table.c"),
    });

    mod.addCSourceFiles(.{ .root = src.path("mpz"), .files = &mpz_files });
    mod.addCSourceFiles(.{ .root = src.path("mpq"), .files = &mpq_files });
    mod.addCSourceFiles(.{ .root = src.path("mpf"), .files = &mpf_files });
    mod.addCSourceFiles(.{ .root = src.path("printf"), .files = &printf_files });
    mod.addCSourceFiles(.{ .root = src.path("scanf"), .files = &scanf_files });
    mod.addCSourceFiles(.{ .root = src.path("rand"), .files = &rand_files });

    mod.addCSourceFiles(.{
        .root = src.path("mpn/generic"),
        .files = &mpn_generic_files,
        .flags = &.{"-DLONGLONG_STANDALONE"},
    });

    // The 5 OPERATION_-templated mpn files are compiled once per operation,
    // each producing a distinct object via a distinct -DOPERATION_* flag.
    addOpFiles(b, mod, src, "mpn/generic/sec_div.c", &.{ "sec_div_qr", "sec_div_r" });
    addOpFiles(b, mod, src, "mpn/generic/popham.c", &.{ "popcount", "hamdist" });
    addOpFiles(b, mod, src, "mpn/generic/sec_pi1_div.c", &.{ "sec_pi1_div_qr", "sec_pi1_div_r" });
    addOpFiles(b, mod, src, "mpn/generic/logops_n.c", &.{ "and_n", "andn_n", "nand_n", "ior_n", "iorn_n", "nior_n", "xor_n", "xnor_n" });
    addOpFiles(b, mod, src, "mpn/generic/sec_aors_1.c", &.{ "sec_add_1", "sec_sub_1" });

    b.installArtifact(lib);
}

fn addOpFiles(
    b: *std.Build,
    mod: *std.Build.Module,
    src: *std.Build.Dependency,
    file: []const u8,
    ops: []const []const u8,
) void {
    for (ops) |op| {
        mod.addCSourceFile(.{
            .file = src.path(file),
            .flags = &.{ "-DLONGLONG_STANDALONE", b.fmt("-DOPERATION_{s}", .{op}) },
        });
    }
}

fn genTable(
    b: *std.Build,
    src: *std.Build.Dependency,
    name: []const u8,
    maybe_arg: ?[]const u8,
    maybe_nail_bits: ?i64,
    maybe_limb_bits: ?i64,
) !std.Build.LazyPath {
    const exe = b.addExecutable(.{
        .name = name,
        .root_module = b.createModule(.{
            .target = b.graph.host,
            .optimize = .ReleaseSafe,
        }),
    });
    exe.root_module.link_libc = true;
    exe.root_module.addCSourceFile(.{ .file = src.path(b.fmt("{s}.c", .{name})) });
    const run = b.addRunArtifact(exe);
    if (maybe_arg) |arg| run.addArg(arg);
    if (maybe_limb_bits) |lb| run.addArg(b.fmt("{d}", .{lb}));
    if (maybe_nail_bits) |nb| run.addArg(b.fmt("{d}", .{nb}));
    return run.captureStdOut(.{});
}

fn makeConfigHeader(
    b: *std.Build,
    src: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
) *std.Build.Step.ConfigHeader {
    const t = target.result;
    return b.addConfigHeader(.{
        .style = .{ .autoconf_undef = src.path("config.in") },
        .include_path = "config.h",
    }, .{
        .AC_APPLE_UNIVERSAL_BUILD = null,
        .GMP_MPARAM_H_SUGGEST = null,
        .HAVE_ALARM = 1,
        .HAVE_ALLOCA = 1,
        .HAVE_ALLOCA_H = 1,
        .HAVE_ATTRIBUTE_CONST = 1,
        .HAVE_ATTRIBUTE_MALLOC = 1,
        .HAVE_ATTRIBUTE_MODE = 1,
        .HAVE_ATTRIBUTE_NORETURN = 1,
        .HAVE_ATTR_GET = null,
        .HAVE_CALLING_CONVENTIONS = null,
        .HAVE_CLOCK = 1,
        .HAVE_CLOCK_GETTIME = 1,
        .HAVE_CPUTIME = null,
        .HAVE_DECL_FGETC = 1,
        .HAVE_DECL_FSCANF = 1,
        .HAVE_DECL_OPTARG = 1,
        .HAVE_DECL_SYS_ERRLIST = 1,
        .HAVE_DECL_SYS_NERR = 1,
        .HAVE_DECL_UNGETC = 1,
        .HAVE_DECL_VFPRINTF = 1,
        .HAVE_DLFCN_H = 1,
        .HAVE_DOUBLE_CRAY_CFP = null,
        .HAVE_DOUBLE_IEEE_BIG_ENDIAN = null,
        .HAVE_DOUBLE_IEEE_LITTLE_ENDIAN = 1,
        .HAVE_DOUBLE_IEEE_LITTLE_SWAPPED = null,
        .HAVE_DOUBLE_VAX_D = null,
        .HAVE_DOUBLE_VAX_G = null,
        .HAVE_FCNTL_H = 1,
        .HAVE_FLOAT_H = 1,
        .HAVE_GETPAGESIZE = 1,
        .HAVE_GETRUSAGE = 1,
        .HAVE_GETSYSINFO = null,
        .HAVE_GETTIMEOFDAY = 1,
        .HAVE_HIDDEN_ALIAS = 1,
        .HAVE_HOST_CPU_FAMILY_alpha = null,
        .HAVE_HOST_CPU_FAMILY_m68k = null,
        .HAVE_HOST_CPU_FAMILY_power = null,
        .HAVE_HOST_CPU_FAMILY_powerpc = null,
        .HAVE_HOST_CPU_FAMILY_x86 = has(t.cpu.arch == .x86),
        .HAVE_HOST_CPU_FAMILY_x86_64 = has(t.cpu.arch == .x86_64),
        .HAVE_HOST_CPU_alphaev67 = null,
        .HAVE_HOST_CPU_alphaev68 = null,
        .HAVE_HOST_CPU_alphaev7 = null,
        .HAVE_HOST_CPU_bobcat = null,
        .HAVE_HOST_CPU_broadwell = null,
        .HAVE_HOST_CPU_bulldozer = null,
        .HAVE_HOST_CPU_core2 = null,
        .HAVE_HOST_CPU_excavator = null,
        .HAVE_HOST_CPU_goldmont = null,
        .HAVE_HOST_CPU_haswell = null,
        .HAVE_HOST_CPU_i386 = null,
        .HAVE_HOST_CPU_i586 = null,
        .HAVE_HOST_CPU_i686 = null,
        .HAVE_HOST_CPU_ivybridge = null,
        .HAVE_HOST_CPU_jaguar = null,
        .HAVE_HOST_CPU_k10 = null,
        .HAVE_HOST_CPU_k8 = null,
        .HAVE_HOST_CPU_m68020 = null,
        .HAVE_HOST_CPU_m68030 = null,
        .HAVE_HOST_CPU_m68040 = null,
        .HAVE_HOST_CPU_m68060 = null,
        .HAVE_HOST_CPU_m68360 = null,
        .HAVE_HOST_CPU_nehalem = null,
        .HAVE_HOST_CPU_pentium = null,
        .HAVE_HOST_CPU_pentium2 = null,
        .HAVE_HOST_CPU_pentium3 = null,
        .HAVE_HOST_CPU_pentium4 = null,
        .HAVE_HOST_CPU_pentiummmx = null,
        .HAVE_HOST_CPU_pentiumpro = null,
        .HAVE_HOST_CPU_piledriver = null,
        .HAVE_HOST_CPU_powerpc604 = null,
        .HAVE_HOST_CPU_powerpc604e = null,
        .HAVE_HOST_CPU_powerpc7400 = null,
        .HAVE_HOST_CPU_powerpc750 = null,
        .HAVE_HOST_CPU_s390_z10 = null,
        .HAVE_HOST_CPU_s390_z13 = null,
        .HAVE_HOST_CPU_s390_z14 = null,
        .HAVE_HOST_CPU_s390_z15 = null,
        .HAVE_HOST_CPU_s390_z196 = null,
        .HAVE_HOST_CPU_s390_z9 = null,
        .HAVE_HOST_CPU_s390_z900 = null,
        .HAVE_HOST_CPU_s390_z990 = null,
        .HAVE_HOST_CPU_s390_zarch = null,
        .HAVE_HOST_CPU_sandybridge = null,
        .HAVE_HOST_CPU_silvermont = null,
        .HAVE_HOST_CPU_skylake = null,
        .HAVE_HOST_CPU_steamroller = null,
        .HAVE_HOST_CPU_supersparc = null,
        .HAVE_HOST_CPU_tremont = null,
        .HAVE_HOST_CPU_westmere = null,
        .HAVE_HOST_CPU_zen = null,
        .HAVE_INTMAX_T = 1,
        .HAVE_INTPTR_T = 1,
        .HAVE_INTTYPES_H = 1,
        .HAVE_INVENT_H = null,
        .HAVE_LANGINFO_H = 1,
        .HAVE_LIMB_BIG_ENDIAN = null,
        .HAVE_LIMB_LITTLE_ENDIAN = 1,
        .HAVE_LOCALECONV = 1,
        .HAVE_LOCALE_H = 1,
        .HAVE_LONG_DOUBLE = 1,
        .HAVE_LONG_LONG = 1,
        .HAVE_MACHINE_HAL_SYSINFO_H = null,
        .HAVE_MEMORY_H = 1,
        .HAVE_MEMSET = 1,
        .HAVE_MMAP = 1,
        .HAVE_MPROTECT = 1,
        .HAVE_NATIVE_mpn_add_n = null,
        .HAVE_NATIVE_mpn_add_n_sub_n = null,
        .HAVE_NATIVE_mpn_add_nc = null,
        .HAVE_NATIVE_mpn_addaddmul_1msb0 = null,
        .HAVE_NATIVE_mpn_addlsh1_n = null,
        .HAVE_NATIVE_mpn_addlsh1_n_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh1_n_ip2 = null,
        .HAVE_NATIVE_mpn_addlsh1_nc = null,
        .HAVE_NATIVE_mpn_addlsh1_nc_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh1_nc_ip2 = null,
        .HAVE_NATIVE_mpn_addlsh2_n = null,
        .HAVE_NATIVE_mpn_addlsh2_n_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh2_n_ip2 = null,
        .HAVE_NATIVE_mpn_addlsh2_nc = null,
        .HAVE_NATIVE_mpn_addlsh2_nc_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh2_nc_ip2 = null,
        .HAVE_NATIVE_mpn_addlsh_n = null,
        .HAVE_NATIVE_mpn_addlsh_n_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh_n_ip2 = null,
        .HAVE_NATIVE_mpn_addlsh_nc = null,
        .HAVE_NATIVE_mpn_addlsh_nc_ip1 = null,
        .HAVE_NATIVE_mpn_addlsh_nc_ip2 = null,
        .HAVE_NATIVE_mpn_addmul_1c = null,
        .HAVE_NATIVE_mpn_addmul_2 = null,
        .HAVE_NATIVE_mpn_addmul_2s = null,
        .HAVE_NATIVE_mpn_addmul_3 = null,
        .HAVE_NATIVE_mpn_addmul_4 = null,
        .HAVE_NATIVE_mpn_addmul_5 = null,
        .HAVE_NATIVE_mpn_addmul_6 = null,
        .HAVE_NATIVE_mpn_addmul_7 = null,
        .HAVE_NATIVE_mpn_addmul_8 = null,
        .HAVE_NATIVE_mpn_and_n = null,
        .HAVE_NATIVE_mpn_andn_n = null,
        .HAVE_NATIVE_mpn_bdiv_dbm1c = null,
        .HAVE_NATIVE_mpn_bdiv_q_1 = null,
        .HAVE_NATIVE_mpn_cnd_add_n = null,
        .HAVE_NATIVE_mpn_cnd_sub_n = null,
        .HAVE_NATIVE_mpn_com = null,
        .HAVE_NATIVE_mpn_copyd = null,
        .HAVE_NATIVE_mpn_copyi = null,
        .HAVE_NATIVE_mpn_div_qr_1n_pi1 = null,
        .HAVE_NATIVE_mpn_div_qr_2 = null,
        .HAVE_NATIVE_mpn_divexact_1 = null,
        .HAVE_NATIVE_mpn_divexact_by3c = null,
        .HAVE_NATIVE_mpn_divrem_1 = null,
        .HAVE_NATIVE_mpn_divrem_1c = null,
        .HAVE_NATIVE_mpn_divrem_2 = null,
        .HAVE_NATIVE_mpn_gcd_1 = null,
        .HAVE_NATIVE_mpn_gcd_11 = null,
        .HAVE_NATIVE_mpn_gcd_22 = null,
        .HAVE_NATIVE_mpn_hamdist = null,
        .HAVE_NATIVE_mpn_invert_limb = null,
        .HAVE_NATIVE_mpn_ior_n = null,
        .HAVE_NATIVE_mpn_iorn_n = null,
        .HAVE_NATIVE_mpn_lshift = null,
        .HAVE_NATIVE_mpn_lshiftc = null,
        .HAVE_NATIVE_mpn_lshsub_n = null,
        .HAVE_NATIVE_mpn_mod_1 = null,
        .HAVE_NATIVE_mpn_mod_1_1p = null,
        .HAVE_NATIVE_mpn_mod_1c = null,
        .HAVE_NATIVE_mpn_mod_1s_2p = null,
        .HAVE_NATIVE_mpn_mod_1s_4p = null,
        .HAVE_NATIVE_mpn_mod_34lsub1 = null,
        .HAVE_NATIVE_mpn_modexact_1_odd = null,
        .HAVE_NATIVE_mpn_modexact_1c_odd = null,
        .HAVE_NATIVE_mpn_mul_1 = null,
        .HAVE_NATIVE_mpn_mul_1c = null,
        .HAVE_NATIVE_mpn_mul_2 = null,
        .HAVE_NATIVE_mpn_mul_3 = null,
        .HAVE_NATIVE_mpn_mul_4 = null,
        .HAVE_NATIVE_mpn_mul_5 = null,
        .HAVE_NATIVE_mpn_mul_6 = null,
        .HAVE_NATIVE_mpn_mul_basecase = null,
        .HAVE_NATIVE_mpn_mullo_basecase = null,
        .HAVE_NATIVE_mpn_nand_n = null,
        .HAVE_NATIVE_mpn_nior_n = null,
        .HAVE_NATIVE_mpn_pi1_bdiv_q_1 = null,
        .HAVE_NATIVE_mpn_popcount = null,
        .HAVE_NATIVE_mpn_preinv_divrem_1 = null,
        .HAVE_NATIVE_mpn_preinv_mod_1 = null,
        .HAVE_NATIVE_mpn_redc_1 = null,
        .HAVE_NATIVE_mpn_redc_2 = null,
        .HAVE_NATIVE_mpn_rsblsh1_n = null,
        .HAVE_NATIVE_mpn_rsblsh1_nc = null,
        .HAVE_NATIVE_mpn_rsblsh2_n = null,
        .HAVE_NATIVE_mpn_rsblsh2_nc = null,
        .HAVE_NATIVE_mpn_rsblsh_n = null,
        .HAVE_NATIVE_mpn_rsblsh_nc = null,
        .HAVE_NATIVE_mpn_rsh1add_n = null,
        .HAVE_NATIVE_mpn_rsh1add_nc = null,
        .HAVE_NATIVE_mpn_rsh1sub_n = null,
        .HAVE_NATIVE_mpn_rsh1sub_nc = null,
        .HAVE_NATIVE_mpn_rshift = null,
        .HAVE_NATIVE_mpn_sbpi1_bdiv_r = null,
        .HAVE_NATIVE_mpn_sqr_basecase = null,
        .HAVE_NATIVE_mpn_sqr_diag_addlsh1 = null,
        .HAVE_NATIVE_mpn_sqr_diagonal = null,
        .HAVE_NATIVE_mpn_sub_n = null,
        .HAVE_NATIVE_mpn_sub_nc = null,
        .HAVE_NATIVE_mpn_sublsh1_n = null,
        .HAVE_NATIVE_mpn_sublsh1_n_ip1 = null,
        .HAVE_NATIVE_mpn_sublsh1_nc = null,
        .HAVE_NATIVE_mpn_sublsh1_nc_ip1 = null,
        .HAVE_NATIVE_mpn_sublsh2_n = null,
        .HAVE_NATIVE_mpn_sublsh2_n_ip1 = null,
        .HAVE_NATIVE_mpn_sublsh2_nc = null,
        .HAVE_NATIVE_mpn_sublsh2_nc_ip1 = null,
        .HAVE_NATIVE_mpn_sublsh_n = null,
        .HAVE_NATIVE_mpn_sublsh_n_ip1 = null,
        .HAVE_NATIVE_mpn_sublsh_nc = null,
        .HAVE_NATIVE_mpn_sublsh_nc_ip1 = null,
        .HAVE_NATIVE_mpn_submul_1c = null,
        .HAVE_NATIVE_mpn_tabselect = null,
        .HAVE_NATIVE_mpn_udiv_qrnnd = null,
        .HAVE_NATIVE_mpn_udiv_qrnnd_r = null,
        .HAVE_NATIVE_mpn_umul_ppmm = null,
        .HAVE_NATIVE_mpn_umul_ppmm_r = null,
        .HAVE_NATIVE_mpn_xnor_n = null,
        .HAVE_NATIVE_mpn_xor_n = null,
        .HAVE_NL_LANGINFO = 1,
        .HAVE_NL_TYPES_H = 1,
        .HAVE_OBSTACK_VPRINTF = 1,
        .HAVE_POPEN = 1,
        .HAVE_PROCESSOR_INFO = null,
        .HAVE_PSP_ITICKSPERCLKTICK = null,
        .HAVE_PSTAT_GETPROCESSOR = null,
        .HAVE_PTRDIFF_T = 1,
        .HAVE_QUAD_T = 1,
        .HAVE_RAISE = 1,
        .HAVE_READ_REAL_TIME = null,
        .HAVE_SIGACTION = 1,
        .HAVE_SIGALTSTACK = 1,
        .HAVE_SIGSTACK = null,
        .HAVE_SPEED_CYCLECOUNTER = null,
        .HAVE_SSTREAM = null,
        .HAVE_STACK_T = 1,
        .HAVE_STDINT_H = 1,
        .HAVE_STDLIB_H = 1,
        .HAVE_STD__LOCALE = null,
        .HAVE_STRCHR = 1,
        .HAVE_STRERROR = 1,
        .HAVE_STRINGS_H = 1,
        .HAVE_STRING_H = 1,
        .HAVE_STRNLEN = 1,
        .HAVE_STRTOL = 1,
        .HAVE_STRTOUL = 1,
        .HAVE_SYSCONF = 1,
        .HAVE_SYSCTL = null,
        .HAVE_SYSCTLBYNAME = null,
        .HAVE_SYSSGI = null,
        .HAVE_SYS_ATTRIBUTES_H = null,
        .HAVE_SYS_IOGRAPH_H = null,
        .HAVE_SYS_MMAN_H = 1,
        .HAVE_SYS_PARAM_H = 1,
        .HAVE_SYS_PROCESSOR_H = null,
        .HAVE_SYS_PSTAT_H = null,
        .HAVE_SYS_RESOURCE_H = 1,
        .HAVE_SYS_STAT_H = 1,
        .HAVE_SYS_SYSCTL_H = null,
        .HAVE_SYS_SYSINFO_H = 1,
        .HAVE_SYS_SYSSGI_H = null,
        .HAVE_SYS_SYSTEMCFG_H = null,
        .HAVE_SYS_TIMES_H = 1,
        .HAVE_SYS_TIME_H = 1,
        .HAVE_SYS_TYPES_H = 1,
        .HAVE_TIMES = 1,
        .HAVE_UINT_LEAST32_T = 1,
        .HAVE_UNISTD_H = 1,
        .HAVE_VSNPRINTF = 1,
        .HOST_DOS64 = null,
        .LSYM_PREFIX = ".L",
        .LT_OBJDIR = ".libs/",
        .NO_ASM = 1,
        .PACKAGE = "gmp",
        .PACKAGE_BUGREPORT = "gmp-bugs@gmplib.org (see https://gmplib.org/manual/Reporting-Bugs.html)",
        .PACKAGE_NAME = "GNU MP",
        .PACKAGE_STRING = "GNU MP 6.3.0",
        .PACKAGE_TARNAME = "gmp",
        .PACKAGE_URL = "http://www.gnu.org/software/gmp/",
        .PACKAGE_VERSION = "6.3.0",
        .RETSIGTYPE = .void,
        .SIZEOF_MP_LIMB_T = 8,
        .SIZEOF_UNSIGNED = 4,
        .SIZEOF_UNSIGNED_LONG = 8,
        .SIZEOF_UNSIGNED_SHORT = 2,
        .SIZEOF_VOID_P = 8,
        .SSCANF_WRITABLE_INPUT = null,
        .STDC_HEADERS = 1,
        .TIME_WITH_SYS_TIME = 1,
        .TUNE_SQR_TOOM2_MAX = .SQR_TOOM2_MAX_GENERIC,
        .VERSION = "6.3.0",
        .WANT_ASSERT = null,
        .WANT_FAKE_CPUID = null,
        .WANT_FAT_BINARY = null,
        .WANT_FFT = 1,
        .WANT_OLD_FFT_FULL = null,
        .WANT_PROFILING_GPROF = null,
        .WANT_PROFILING_INSTRUMENT = null,
        .WANT_PROFILING_PROF = null,
        .WANT_TMP_ALLOCA = 1,
        .WANT_TMP_DEBUG = null,
        .WANT_TMP_NOTREENTRANT = null,
        .WANT_TMP_REENTRANT = null,
        .WORDS_BIGENDIAN = null,
        .X86_ASM_MULX = null,
        .YYTEXT_POINTER = 1,
        .@"inline" = null,
        .@"restrict" = .__restrict,
        .@"volatile" = null,
    });
}

fn has(pred: bool) ?u1 {
    return if (pred) 1 else null;
}

const top_level_files = [_][]const u8{
    "assert.c",
    "compat.c",
    "errno.c",
    "extract-dbl.c",
    "invalid.c",
    "memory.c",
    "mp_bpl.c",
    "mp_clz_tab.c",
    "mp_dv_tab.c",
    "mp_get_fns.c",
    "mp_minv_tab.c",
    "mp_set_fns.c",
    "nextprime.c",
    "primesieve.c",
    "tal-reent.c",
    "version.c",
};

const mpz_files = [_][]const u8{
    "2fac_ui.c",     "abs.c",         "add.c",         "add_ui.c",
    "and.c",         "aorsmul.c",     "aorsmul_i.c",   "array_init.c",
    "bin_ui.c",      "bin_uiui.c",    "cdiv_q.c",      "cdiv_q_ui.c",
    "cdiv_qr.c",     "cdiv_qr_ui.c",  "cdiv_r.c",      "cdiv_r_ui.c",
    "cdiv_ui.c",     "cfdiv_q_2exp.c", "cfdiv_r_2exp.c", "clear.c",
    "clears.c",      "clrbit.c",      "cmp.c",         "cmp_d.c",
    "cmp_si.c",      "cmp_ui.c",      "cmpabs.c",      "cmpabs_d.c",
    "cmpabs_ui.c",   "com.c",         "combit.c",      "cong.c",
    "cong_2exp.c",   "cong_ui.c",     "dive_ui.c",     "divegcd.c",
    "divexact.c",    "divis.c",       "divis_2exp.c",  "divis_ui.c",
    "dump.c",        "export.c",      "fac_ui.c",      "fdiv_q.c",
    "fdiv_q_ui.c",   "fdiv_qr.c",     "fdiv_qr_ui.c",  "fdiv_r.c",
    "fdiv_r_ui.c",   "fdiv_ui.c",     "fib2_ui.c",     "fib_ui.c",
    "fits_sint.c",   "fits_slong.c",  "fits_sshort.c", "fits_uint.c",
    "fits_ulong.c",  "fits_ushort.c", "gcd.c",         "gcd_ui.c",
    "gcdext.c",      "get_d.c",       "get_d_2exp.c",  "get_si.c",
    "get_str.c",     "get_ui.c",      "getlimbn.c",    "hamdist.c",
    "import.c",      "init.c",        "init2.c",       "inits.c",
    "inp_raw.c",     "inp_str.c",     "invert.c",      "ior.c",
    "iset.c",        "iset_d.c",      "iset_si.c",     "iset_str.c",
    "iset_ui.c",     "jacobi.c",      "kronsz.c",      "kronuz.c",
    "kronzs.c",      "kronzu.c",      "lcm.c",         "lcm_ui.c",
    "limbs_finish.c", "limbs_modify.c", "limbs_read.c", "limbs_write.c",
    "lucmod.c",      "lucnum2_ui.c",  "lucnum_ui.c",   "mfac_uiui.c",
    "millerrabin.c", "mod.c",         "mul.c",         "mul_2exp.c",
    "mul_si.c",      "mul_ui.c",      "n_pow_ui.c",    "neg.c",
    "nextprime.c",   "oddfac_1.c",    "out_raw.c",     "out_str.c",
    "perfpow.c",     "perfsqr.c",     "popcount.c",    "pow_ui.c",
    "powm.c",        "powm_sec.c",    "powm_ui.c",     "pprime_p.c",
    "primorial_ui.c", "prodlimbs.c",  "random.c",      "random2.c",
    "realloc.c",     "realloc2.c",    "remove.c",      "roinit_n.c",
    "root.c",        "rootrem.c",     "rrandomb.c",    "scan0.c",
    "scan1.c",       "set.c",         "set_d.c",       "set_f.c",
    "set_q.c",       "set_si.c",      "set_str.c",     "set_ui.c",
    "setbit.c",      "size.c",        "sizeinbase.c",  "sqrt.c",
    "sqrtrem.c",     "stronglucas.c", "sub.c",         "sub_ui.c",
    "swap.c",        "tdiv_q.c",      "tdiv_q_2exp.c", "tdiv_q_ui.c",
    "tdiv_qr.c",     "tdiv_qr_ui.c",  "tdiv_r.c",      "tdiv_r_2exp.c",
    "tdiv_r_ui.c",   "tdiv_ui.c",     "tstbit.c",      "ui_pow_ui.c",
    "ui_sub.c",      "urandomb.c",    "urandomm.c",    "xor.c",
};

const mpq_files = [_][]const u8{
    "abs.c",          "aors.c",   "canonicalize.c", "clear.c",
    "clears.c",       "cmp.c",    "cmp_si.c",       "cmp_ui.c",
    "div.c",          "equal.c",  "get_d.c",        "get_den.c",
    "get_num.c",      "get_str.c", "init.c",        "inits.c",
    "inp_str.c",      "inv.c",    "md_2exp.c",      "mul.c",
    "neg.c",          "out_str.c", "set.c",         "set_d.c",
    "set_den.c",      "set_f.c",  "set_num.c",      "set_si.c",
    "set_str.c",      "set_ui.c", "set_z.c",        "swap.c",
};

const mpf_files = [_][]const u8{
    "abs.c",         "add.c",        "add_ui.c",      "ceilfloor.c",
    "clear.c",       "clears.c",     "cmp.c",         "cmp_d.c",
    "cmp_si.c",      "cmp_ui.c",     "cmp_z.c",       "div.c",
    "div_2exp.c",    "div_ui.c",     "dump.c",        "eq.c",
    "fits_sint.c",   "fits_slong.c", "fits_sshort.c", "fits_uint.c",
    "fits_ulong.c",  "fits_ushort.c", "get_d.c",      "get_d_2exp.c",
    "get_dfl_prec.c", "get_prc.c",   "get_si.c",      "get_str.c",
    "get_ui.c",      "init.c",       "init2.c",       "inits.c",
    "inp_str.c",     "int_p.c",      "iset.c",        "iset_d.c",
    "iset_si.c",     "iset_str.c",   "iset_ui.c",     "mul.c",
    "mul_2exp.c",    "mul_ui.c",     "neg.c",         "out_str.c",
    "pow_ui.c",      "random2.c",    "reldiff.c",     "set.c",
    "set_d.c",       "set_dfl_prec.c", "set_prc.c",   "set_prc_raw.c",
    "set_q.c",       "set_si.c",     "set_str.c",     "set_ui.c",
    "set_z.c",       "size.c",       "sqrt.c",        "sqrt_ui.c",
    "sub.c",         "sub_ui.c",     "swap.c",        "trunc.c",
    "ui_div.c",      "ui_sub.c",     "urandomb.c",
};

const printf_files = [_][]const u8{
    "asprintf.c",    "asprntffuns.c", "doprnt.c",      "doprntf.c",
    "doprnti.c",     "fprintf.c",     "obprintf.c",    "obprntffuns.c",
    "obvprintf.c",   "printf.c",      "printffuns.c",  "repl-vsnprintf.c",
    "snprintf.c",    "snprntffuns.c", "sprintf.c",     "sprintffuns.c",
    "vasprintf.c",   "vfprintf.c",    "vprintf.c",     "vsnprintf.c",
    "vsprintf.c",
};

const scanf_files = [_][]const u8{
    "doscan.c",    "fscanf.c",     "fscanffuns.c", "scanf.c",
    "sscanf.c",    "sscanffuns.c", "vfscanf.c",    "vscanf.c",
    "vsscanf.c",
};

const rand_files = [_][]const u8{
    "rand.c",      "randbui.c",  "randclr.c",   "randdef.c",
    "randiset.c",  "randlc2s.c", "randlc2x.c",  "randmt.c",
    "randmts.c",   "randmui.c",  "rands.c",     "randsd.c",
    "randsdui.c",
};

const mpn_generic_files = [_][]const u8{
    "add.c",                          "add_1.c",
    "add_err1_n.c",                   "add_err2_n.c",
    "add_err3_n.c",                   "add_n.c",
    "add_n_sub_n.c",                  "addmul_1.c",
    "bdiv_dbm1c.c",                   "bdiv_q.c",
    "bdiv_q_1.c",                     "bdiv_qr.c",
    "binvert.c",                      "broot.c",
    "brootinv.c",                     "bsqrt.c",
    "bsqrtinv.c",                     "cmp.c",
    "cnd_add_n.c",                    "cnd_sub_n.c",
    "cnd_swap.c",                     "com.c",
    "comb_tables.c",                  "compute_powtab.c",
    "copyd.c",                        "copyi.c",
    "dcpi1_bdiv_q.c",                 "dcpi1_bdiv_qr.c",
    "dcpi1_div_q.c",                  "dcpi1_div_qr.c",
    "dcpi1_divappr_q.c",              "div_q.c",
    "div_qr_1.c",                     "div_qr_1n_pi1.c",
    "div_qr_1n_pi2.c",                "div_qr_1u_pi2.c",
    "div_qr_2.c",                     "div_qr_2n_pi1.c",
    "div_qr_2u_pi1.c",                "dive_1.c",
    "diveby3.c",                      "divexact.c",
    "divis.c",                        "divrem.c",
    "divrem_1.c",                     "divrem_2.c",
    "dump.c",                         "fib2_ui.c",
    "fib2m.c",                        "gcd.c",
    "gcd_1.c",                        "gcd_11.c",
    "gcd_22.c",                       "gcd_subdiv_step.c",
    "gcdext.c",                       "gcdext_1.c",
    "gcdext_lehmer.c",                "get_d.c",
    "get_str.c",                      "hgcd.c",
    "hgcd2.c",                        "hgcd2_jacobi.c",
    "hgcd_appr.c",                    "hgcd_jacobi.c",
    "hgcd_matrix.c",                  "hgcd_reduce.c",
    "hgcd_step.c",                    "invert.c",
    "invertappr.c",                   "jacbase.c",
    "jacobi.c",                       "jacobi_2.c",
    "lshift.c",                       "lshiftc.c",
    "matrix22_mul.c",                 "matrix22_mul1_inverse_vector.c",
    "mod_1.c",                        "mod_1_1.c",
    "mod_1_2.c",                      "mod_1_3.c",
    "mod_1_4.c",                      "mod_34lsub1.c",
    "mode1o.c",                       "mu_bdiv_q.c",
    "mu_bdiv_qr.c",                   "mu_div_q.c",
    "mu_div_qr.c",                    "mu_divappr_q.c",
    "mul.c",                          "mul_1.c",
    "mul_basecase.c",                 "mul_fft.c",
    "mul_n.c",                        "mullo_basecase.c",
    "mullo_n.c",                      "mulmid.c",
    "mulmid_basecase.c",              "mulmid_n.c",
    "mulmod_bknp1.c",                 "mulmod_bnm1.c",
    "neg.c",                          "nussbaumer_mul.c",
    "perfpow.c",                      "perfsqr.c",
    "pow_1.c",                        "powlo.c",
    "powm.c",                         "pre_divrem_1.c",
    "pre_mod_1.c",                    "random.c",
    "random2.c",                      "redc_1.c",
    "redc_2.c",                       "redc_n.c",
    "remove.c",                       "rootrem.c",
    "rshift.c",                       "sbpi1_bdiv_q.c",
    "sbpi1_bdiv_qr.c",                "sbpi1_bdiv_r.c",
    "sbpi1_div_q.c",                  "sbpi1_div_qr.c",
    "sbpi1_divappr_q.c",              "scan0.c",
    "scan1.c",                        "sec_invert.c",
    "sec_mul.c",                      "sec_powm.c",
    "sec_sqr.c",                      "sec_tabselect.c",
    "set_str.c",                      "sizeinbase.c",
    "sqr.c",                          "sqr_basecase.c",
    "sqrlo.c",                        "sqrlo_basecase.c",
    "sqrmod_bnm1.c",                  "sqrtrem.c",
    "strongfibo.c",                   "sub.c",
    "sub_1.c",                        "sub_err1_n.c",
    "sub_err2_n.c",                   "sub_err3_n.c",
    "sub_n.c",                        "submul_1.c",
    "tdiv_qr.c",                      "toom22_mul.c",
    "toom2_sqr.c",                    "toom32_mul.c",
    "toom33_mul.c",                   "toom3_sqr.c",
    "toom42_mul.c",                   "toom42_mulmid.c",
    "toom43_mul.c",                   "toom44_mul.c",
    "toom4_sqr.c",                    "toom52_mul.c",
    "toom53_mul.c",                   "toom54_mul.c",
    "toom62_mul.c",                   "toom63_mul.c",
    "toom6_sqr.c",                    "toom6h_mul.c",
    "toom8_sqr.c",                    "toom8h_mul.c",
    "toom_couple_handling.c",         "toom_eval_dgr3_pm1.c",
    "toom_eval_dgr3_pm2.c",           "toom_eval_pm1.c",
    "toom_eval_pm2.c",                "toom_eval_pm2exp.c",
    "toom_eval_pm2rexp.c",            "toom_interpolate_12pts.c",
    "toom_interpolate_16pts.c",       "toom_interpolate_5pts.c",
    "toom_interpolate_6pts.c",        "toom_interpolate_7pts.c",
    "toom_interpolate_8pts.c",        "trialdiv.c",
    "zero.c",                         "zero_p.c",
};
