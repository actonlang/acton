const std = @import("std");
const builtin = @import("builtin");

// Helper for Zig 0.13/0.14 API differences.
fn targetIsDarwin(t: std.Target) bool {
    const is_zig_0_14 = comptime builtin.zig_version.order(std.SemanticVersion.parse("0.14.0") catch unreachable) != .lt;
    if (is_zig_0_14) {
        return t.os.tag.isDarwin();
    } else {
        return t.isDarwin();
    }
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const t = target.result;
    const optimize = b.standardOptimizeOption(.{});

    const enable_http = b.option(bool, "http", "enable HTTP/websocket support") orelse true;
    const enable_keychain = b.option(bool, "keychain", "enable keychain support on platforms that support it") orelse true;
    const tls_lib = b.option([]const u8, "tlslib", "TLS implementation library (openssl|mbedtls)") orelse "mbedtls";

    const use_openssl = std.mem.eql(u8, tls_lib, "openssl");
    const use_mbedtls = std.mem.eql(u8, tls_lib, "mbedtls");
    if (!use_openssl and !use_mbedtls) {
        @panic("Unsupported TLS library");
    }

    const dep_libuv = b.dependency("libuv", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libmbedtls = if (use_mbedtls) b.dependency("libmbedtls", .{
        .target = target,
        .optimize = optimize,
    }) else null;

    const lib = b.addLibrary(.{
        .name = "tlsuv",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    var cflags = std.ArrayList([]const u8).empty;
    defer cflags.deinit(b.allocator);
    cflags.append(b.allocator, "-std=c99") catch unreachable;
    if (t.os.tag == .windows) {
        cflags.append(b.allocator, "-Wno-error=macro-redefined") catch unreachable;
    }

    const base_sources = [_][]const u8{
        "src/tlsuv.c",
        "src/um_debug.c",
        "src/base64.c",
        "src/tls_engine.c",
        "src/p11.c",
        "src/socket.c",
        "src/connector.c",
        "src/alloc.c",
        "src/keychain.c",
        "src/url.c",
    };

    lib.root_module.addCSourceFiles(.{
        .files = &base_sources,
        .flags = cflags.items,
    });

    if (enable_keychain) {
        if (targetIsDarwin(t)) {
            lib.root_module.addCSourceFile(.{
                .file = b.path("src/apple/keychain.c"),
                .flags = cflags.items,
            });
            lib.root_module.linkFramework("CoreFoundation", .{});
            lib.root_module.linkFramework("Security", .{});
        } else if (t.os.tag == .windows) {
            lib.root_module.addCSourceFile(.{
                .file = b.path("src/win32/win32_keychain.c"),
                .flags = cflags.items,
            });
            lib.root_module.linkSystemLibrary("crypt32", .{});
            lib.root_module.linkSystemLibrary("ncrypt", .{});
        }
    }

    if (enable_http) {
        const http_sources = [_][]const u8{
            "src/http.c",
            "src/tcp_src.c",
            "src/websocket.c",
            "src/http_req.c",
            "src/tls_link.c",
            "src/compression.c",
        };
        lib.root_module.addCSourceFiles(.{
            .files = &http_sources,
            .flags = cflags.items,
        });

        const uv_link_sources = [_][]const u8{
            "deps/uv_link_t/src/defaults.c",
            "deps/uv_link_t/src/uv_link_t.c",
            "deps/uv_link_t/src/uv_link_source_t.c",
            "deps/uv_link_t/src/uv_link_observer_t.c",
        };
        lib.root_module.addCSourceFiles(.{
            .files = &uv_link_sources,
            .flags = cflags.items,
        });

        lib.root_module.addIncludePath(b.path("deps/uv_link_t/include"));
        lib.root_module.addIncludePath(b.path("deps/uv_link_t"));
        lib.root_module.linkSystemLibrary("z", .{});
        lib.root_module.linkSystemLibrary("llhttp", .{});
        lib.root_module.addCMacro("TLSUV_HTTP", "1");
    }

    if (use_openssl) {
        const ssl_sources = [_][]const u8{
            "src/openssl/engine.c",
            "src/openssl/keys.c",
        };
        lib.root_module.addCSourceFiles(.{
            .files = &ssl_sources,
            .flags = cflags.items,
        });
        lib.root_module.linkSystemLibrary("ssl", .{});
        lib.root_module.linkSystemLibrary("crypto", .{});
        lib.root_module.addCMacro("USE_OPENSSL", "1");
        lib.root_module.addCMacro("TLS_IMPL", "openssl");
    } else if (use_mbedtls) {
        const ssl_sources = [_][]const u8{
            "src/mbedtls/engine.c",
            "src/mbedtls/keys.c",
            "src/mbedtls/mbed_p11.c",
            "src/mbedtls/p11_ecdsa.c",
            "src/mbedtls/p11_rsa.c",
        };
        lib.root_module.addCSourceFiles(.{
            .files = &ssl_sources,
            .flags = cflags.items,
        });
        if (dep_libmbedtls) |mbedtls| {
            lib.root_module.linkLibrary(mbedtls.artifact("mbedcrypto"));
            lib.root_module.linkLibrary(mbedtls.artifact("mbedtls"));
            lib.root_module.linkLibrary(mbedtls.artifact("mbedx509"));
        }
        lib.root_module.addCMacro("USE_MBEDTLS", "1");
        lib.root_module.addCMacro("TLS_IMPL", "mbedtls");
    }

    lib.root_module.addIncludePath(b.path("include"));
    lib.root_module.addIncludePath(b.path("src"));

    lib.root_module.linkLibrary(dep_libuv.artifact("uv"));
    lib.root_module.link_libc = true;

    if (t.os.tag == .windows) {
        lib.root_module.addCMacro("WIN32_LEAN_AND_MEAN", "1");
        lib.root_module.addCMacro("_CRT_SECURE_NO_WARNINGS", "1");
        lib.root_module.addCMacro("_CRT_NONSTDC_NO_DEPRECATE", "1");
        lib.root_module.addCMacro("_WINSOCK_DEPRECATED_NO_WARNINGS", "1");
    }

    if (t.os.tag == .linux) {
        lib.root_module.addCMacro("_POSIX_C_SOURCE", "200112");
        lib.root_module.addCMacro("_GNU_SOURCE", "1");
    }

    lib.root_module.addCMacro("TLSUV_VERSION", "v0.0.0");

    lib.installHeadersDirectory(b.path("include/tlsuv"), "tlsuv", .{});
    b.installArtifact(lib);
}
