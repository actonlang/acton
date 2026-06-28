const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;

    const libcrypto = b.addLibrary(.{
        .name = "mbedcrypto",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) libcrypto.lto = .full;

    const libx509 = b.addLibrary(.{
        .name = "mbedx509",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) libx509.lto = .full;

    const libtls = b.addLibrary(.{
        .name = "mbedtls",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) libtls.lto = .full;

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    libcrypto.root_module.addCSourceFiles(.{
        .files = &.{
            "library/aes.c",
            "library/aesni.c",
            "library/aesce.c",
            "library/aria.c",
            "library/asn1parse.c",
            "library/asn1write.c",
            "library/base64.c",
            "library/bignum.c",
            "library/bignum_core.c",
            "library/bignum_mod.c",
            "library/bignum_mod_raw.c",
            "library/camellia.c",
            "library/ccm.c",
            "library/chacha20.c",
            "library/chachapoly.c",
            "library/cipher.c",
            "library/cipher_wrap.c",
            "library/constant_time.c",
            "library/cmac.c",
            "library/ctr_drbg.c",
            "library/des.c",
            "library/dhm.c",
            "library/ecdh.c",
            "library/ecdsa.c",
            "library/ecjpake.c",
            "library/ecp.c",
            "library/ecp_curves.c",
            "library/entropy.c",
            "library/entropy_poll.c",
            "library/error.c",
            "library/gcm.c",
            "library/hash_info.c",
            "library/hkdf.c",
            "library/hmac_drbg.c",
            "library/lmots.c",
            "library/lms.c",
            "library/md.c",
            "library/md5.c",
            "library/memory_buffer_alloc.c",
            "library/nist_kw.c",
            "library/oid.c",
            "library/padlock.c",
            "library/pem.c",
            "library/pk.c",
            "library/pk_wrap.c",
            "library/pkcs12.c",
            "library/pkcs5.c",
            "library/pkparse.c",
            "library/pkwrite.c",
            "library/platform.c",
            "library/platform_util.c",
            "library/poly1305.c",
            "library/psa_crypto.c",
            "library/psa_crypto_aead.c",
            "library/psa_crypto_cipher.c",
            "library/psa_crypto_client.c",
            "library/psa_crypto_driver_wrappers.c",
            "library/psa_crypto_ecp.c",
            "library/psa_crypto_hash.c",
            "library/psa_crypto_mac.c",
            "library/psa_crypto_pake.c",
            "library/psa_crypto_rsa.c",
            "library/psa_crypto_se.c",
            "library/psa_crypto_slot_management.c",
            "library/psa_crypto_storage.c",
            "library/psa_its_file.c",
            "library/psa_util.c",
            "library/ripemd160.c",
            "library/rsa.c",
            "library/rsa_alt_helpers.c",
            "library/sha1.c",
            "library/sha256.c",
            "library/sha512.c",
            "library/threading.c",
            "library/timing.c",
            "library/version.c",
            "library/version_features.c",
        },
        .flags = flags.items
    });

    libx509.root_module.addCSourceFiles(.{
        .files = &.{
            "library/pkcs7.c",
            "library/x509.c",
            "library/x509_create.c",
            "library/x509_crl.c",
            "library/x509_crt.c",
            "library/x509_csr.c",
            "library/x509write_crt.c",
            "library/x509write_csr.c",
        },
        .flags = flags.items
    });

    libtls.root_module.addCSourceFiles(.{
        .files = &.{
            "library/debug.c",
            "library/mps_reader.c",
            "library/mps_trace.c",
            "library/net_sockets.c",
            "library/ssl_cache.c",
            "library/ssl_ciphersuites.c",
            "library/ssl_client.c",
            "library/ssl_cookie.c",
            "library/ssl_debug_helpers_generated.c",
            "library/ssl_msg.c",
            "library/ssl_ticket.c",
            "library/ssl_tls.c",
            "library/ssl_tls12_client.c",
            "library/ssl_tls12_server.c",
            "library/ssl_tls13_keys.c",
            "library/ssl_tls13_server.c",
            "library/ssl_tls13_client.c",
            "library/ssl_tls13_generic.c",
        },
        .flags = flags.items
    });

    libcrypto.root_module.addIncludePath(b.path("include"));
    libcrypto.root_module.link_libc = true;
    libx509.root_module.addIncludePath(b.path("include"));
    libx509.root_module.link_libc = true;
    libtls.root_module.addIncludePath(b.path("include"));
    libtls.root_module.link_libc = true;

    libtls.installHeadersDirectory(b.path("include/mbedtls"), "mbedtls", .{});
    libtls.installHeadersDirectory(b.path("include/psa"), "psa", .{});

    b.installArtifact(libcrypto);
    b.installArtifact(libx509);
    b.installArtifact(libtls);
}
