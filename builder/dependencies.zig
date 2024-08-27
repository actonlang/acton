pub const packages = struct {
    pub const @"backend" = struct {
        pub const build_root = ".build/sys/backend";
        pub const build_zig = @import("backendbuild.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{
            .{ "libargp", "libargp" },
            .{ "libgc", "libgc" },
            .{ "libnetstring", "libnetstring" },
            .{ "libprotobuf_c", "libprotobuf_c" },
            .{ "libuuid", "libuuid" },
            .{ "libyyjson", "libyyjson" },
        };
    };
    pub const @"base" = struct {
        pub const build_root = ".build/sys/base";
        pub const build_zig = @import("basebuild.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{
            .{ "libbsdnt", "libbsdnt" },
            .{ "libgc", "libgc" },
            .{ "libnetstring", "libnetstring" },
            .{ "libpcre2", "libpcre2" },
            .{ "libutf8proc", "libutf8proc" },
            .{ "libxml2", "libxml2" },
            .{ "libyyjson", "libyyjson" },
        };
    };
    pub const @"libnetstring" = struct {
        pub const build_root = ".build/sys/deps/libnetstring";
        pub const build_zig = @import("deps/libnetstring/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libbsdnt" = struct {
        pub const build_root = ".build/sys/deps/libbsdnt";
        pub const build_zig = @import("deps/libbsdnt/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libprotobuf_c" = struct {
        pub const build_root = ".build/sys/deps/libprotobuf_c";
        pub const build_zig = @import("deps/libprotobuf_c/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libuv" = struct {
        pub const build_root = ".build/sys/deps/libuv";
        pub const build_zig = @import("deps/libuv/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libxml2" = struct {
        pub const build_root = ".build/sys/deps/libxml2";
        pub const build_zig = @import("deps/libxml2/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libgc" = struct {
        pub const build_root = ".build/sys/deps/libgc";
        pub const build_zig = @import("deps/libgc/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libuuid" = struct {
        pub const build_root = ".build/sys/deps/libuuid";
        pub const build_zig = @import("deps/libuuid/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libyyjson" = struct {
        pub const build_root = ".build/sys/deps/libyyjson";
        pub const build_zig = @import("deps/libyyjson/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libargp" = struct {
        pub const build_root = ".build/sys/deps/libargp";
        pub const build_zig = @import("deps/libargp/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libmbedtls" = struct {
        pub const build_root = ".build/sys/deps/mbedtls";
        pub const build_zig = @import("deps/mbedtls/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libsnappy_c" = struct {
        pub const build_root = ".build/sys/deps/libsnappy_c";
        pub const build_zig = @import("deps/libsnappy_c/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libutf8proc" = struct {
        pub const build_root = ".build/sys/deps/libutf8proc";
        pub const build_zig = @import("deps/libutf8proc/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libpcre2" = struct {
        pub const build_root = ".build/sys/deps/pcre2";
        pub const build_zig = @import("deps/pcre2/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
    pub const @"libtlsuv" = struct {
        pub const build_root = ".build/sys/deps/tlsuv";
        pub const build_zig = @import("deps/tlsuv/build.zig");
        pub const deps: []const struct { []const u8, []const u8 } = &.{};
    };
};

pub const root_deps: []const struct { []const u8, []const u8 } = &.{
    .{ "actondb", "backend" },
    .{ "base", "base" },
    .{ "libargp", "libargp" },
    .{ "libbsdnt", "libbsdnt" },
    .{ "libgc", "libgc" },
    .{ "libmbedtls", "libmbedtls" },
    .{ "libnetstring", "libnetstring" },
    .{ "libpcre2", "libpcre2" },
    .{ "libprotobuf_c", "libprotobuf_c" },
    .{ "libsnappy", "libsnappy_c" },
    .{ "libtlsuv", "libtlsuv" },
    .{ "libutf8proc", "libutf8proc" },
    .{ "libuuid", "libuuid" },
    .{ "libuv", "libuv" },
    .{ "libxml2", "libxml2" },
    .{ "libyyjson", "libyyjson" },
};
