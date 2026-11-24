const std = @import("std");

const acton = @import("acton.zig");
const gc = @import("rts/gc.zig");

// Both base64 functions build their result with acton.new_bytes so that it
// looks exactly like bytes built by the C runtime, with a real (non-dangling)
// buffer even when the output is empty.
export fn stdQ_base64Q_encode(data: *acton.bytes) callconv(.c) *acton.bytes {
    const encoder = std.base64.standard.Encoder;
    // For possible Unicode input, bytes and chars may not be 1:1
    const data_len: usize = @intCast(data.nbytes);
    const out_len = encoder.calcSize(data_len);
    const res = acton.new_bytes(data.class, out_len);
    const buffer: []u8 = @constCast(res.str[0..out_len]);
    _ = encoder.encode(buffer, data.str[0..data_len]);
    return res;
}

export fn stdQ_base64Q_decode(data: *acton.bytes) callconv(.c) *acton.bytes {
    const decoder = std.base64.standard.Decoder;
    // Slice the input by its length for the decoder
    const data_len: usize = @intCast(data.nbytes);
    const data_slice = data.str[0..data_len];
    // And then compute the exact number of bytes we need to decode, without padding
    const out_len = decoder.calcSizeForSlice(data_slice) catch {
        acton.raise_ValueError("Invalid base64 input data");
        unreachable; // raise above does longjmp so this is unreachable
    };
    const res = acton.new_bytes(data.class, out_len);
    const buffer: []u8 = @constCast(res.str[0..out_len]);
    // calcSizeForSlice only validates the length and trailing padding, so
    // decode can still fail on characters outside the alphabet or on
    // misplaced padding.
    decoder.decode(buffer, data_slice) catch {
        acton.raise_ValueError("Invalid base64 input data");
        unreachable;
    };
    return res;
}

const flate = std.compress.flate;

fn flateCompress(gpa: std.mem.Allocator, input: []const u8, container: flate.Container) ![]u8 {
    // Text often compresses to less than a quarter, so start there
    var out = try std.Io.Writer.Allocating.initCapacity(gpa, input.len / 4 + 64);
    errdefer out.deinit();
    const window = try gpa.alloc(u8, flate.max_window_len);
    defer gpa.free(window);
    var compress = try flate.Compress.init(&out.writer, window, container, .default);
    try compress.writer.writeAll(input);
    try compress.finish();
    return out.written();
}

// Decompress one stream, which must be all of input
fn flateDecompress(gpa: std.mem.Allocator, input: []const u8, container: flate.Container) ![]u8 {
    var in: std.Io.Reader = .fixed(input);
    const window = try gpa.alloc(u8, flate.max_window_len);
    defer gpa.free(window);
    var decompress: flate.Decompress = .init(&in, container, window);
    const out = try decompress.reader.allocRemaining(gpa, .unlimited);
    if (in.bufferedLen() != 0) {
        return error.TrailingData;
    }
    return out;
}

fn decompressBytes(data: *acton.bytes, container: flate.Container) *acton.bytes {
    const len: usize = @intCast(data.nbytes);
    const out = flateDecompress(gc.atomicAllocator(), data.str[0..len], container) catch |err| {
        if (err == error.OutOfMemory) {
            acton.raise_MemoryError("OOM while decompressing");
        } else {
            acton.raise_ValueError("Invalid compressed data");
        }
        unreachable; // raise above does longjmp so this is unreachable
    };
    const res = acton.new_bytes(data.class, out.len);
    @memcpy(@constCast(res.str[0..out.len]), out);
    return res;
}

fn compressBytes(data: *acton.bytes, container: flate.Container) *acton.bytes {
    const len: usize = @intCast(data.nbytes);
    const out = flateCompress(gc.atomicAllocator(), data.str[0..len], container) catch |err| {
        if (err == error.OutOfMemory) {
            acton.raise_MemoryError("OOM while compressing");
        } else {
            acton.raise_ValueError("Cannot compress the data");
        }
        unreachable; // raise above does longjmp so this is unreachable
    };
    const res = acton.new_bytes(data.class, out.len);
    @memcpy(@constCast(res.str[0..out.len]), out);
    return res;
}

export fn stdQ_compressQ_gzipQ_compress(data: *acton.bytes) callconv(.c) *acton.bytes {
    return compressBytes(data, .gzip);
}

export fn stdQ_compressQ_zlibQ_compress(data: *acton.bytes) callconv(.c) *acton.bytes {
    return compressBytes(data, .zlib);
}

export fn stdQ_compressQ_gzipQ_decompress(data: *acton.bytes) callconv(.c) *acton.bytes {
    return decompressBytes(data, .gzip);
}

export fn stdQ_compressQ_zlibQ_decompress(data: *acton.bytes) callconv(.c) *acton.bytes {
    return decompressBytes(data, .zlib);
}

export fn zig_crypto_hash_md5_init() callconv(.c) *std.crypto.hash.Md5 {
    // Md5 state is only integers and bytes, so it need not be scanned
    const alloc = gc.atomicAllocator();
    const hasher_ptr = alloc.create(std.crypto.hash.Md5) catch {
        unreachable("OOM while allocating Md5 hasher");
    };
    hasher_ptr.* = std.crypto.hash.Md5.init(.{});

    return hasher_ptr;
}

export fn zig_crypto_hash_md5_update(hasher: *std.crypto.hash.Md5, data: *acton.bytes) callconv(.c) void {
    const len: usize = @intCast(data.nbytes); // destination type from context
    const slice = data.str[0..len];
    hasher.update(slice);
}

export fn zig_crypto_hash_md5_finalize(hasher: *std.crypto.hash.Md5, output: *acton.bytes) callconv(.c) void {
    const digest_len = 16;
    const out_slice: *[16]u8 = @as([*]u8, @ptrCast(@constCast(output.str)))[0..digest_len];
    hasher.final(out_slice);
}

// SHA-256 for std.crypto.hash.sha256. The C side in that module allocates the
// state of a hasher and the 32 bytes of a digest, and passes data as a pointer
// and a length. The data of an empty bytes value may be null, so it goes
// through optionalSlice.
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Size of the state of a hasher. The C side allocates it with
/// acton_malloc_atomic, so that it comes from the same allocator as the Hasher
/// object that points to it: a Hasher made while a module is initialized is
/// not memory of the collector, and a state on the collector's heap that only
/// such an object refers to would be freed.
export fn zig_crypto_hash_sha256_size() callconv(.c) usize {
    return @sizeOf(Sha256);
}

/// Start with no data, in storage of zig_crypto_hash_sha256_size() bytes. The
/// storage must have the alignment of Sha256, 16 bytes, which
/// acton_malloc_atomic gives.
export fn zig_crypto_hash_sha256_init(storage: *anyopaque) callconv(.c) void {
    const hasher: *Sha256 = @ptrCast(@alignCast(storage));
    hasher.* = Sha256.init(.{});
}

export fn zig_crypto_hash_sha256_update(hasher: *Sha256, data: ?[*]const u8, len: usize) callconv(.c) void {
    hasher.update(optionalSlice(data, len));
}

// Writes the digest of the data given so far. The padding goes into a copy of
// the state, so the hasher itself can take more data afterwards.
export fn zig_crypto_hash_sha256_finalize(hasher: *const Sha256, out: *[Sha256.digest_length]u8) callconv(.c) void {
    out.* = hasher.peek();
}

export fn zig_crypto_hash_sha256_hash(data: ?[*]const u8, len: usize, out: *[Sha256.digest_length]u8) callconv(.c) void {
    Sha256.hash(optionalSlice(data, len), out, .{});
}

// AES-GCM for std.crypto.aead.aes_gcm. The C side in that module checks the
// key length (16 bytes selects AES-128, 32 bytes AES-256) and that the message
// fits in GCM's length limit, allocates the output and passes every buffer as
// a pointer, with a length where it varies. The plaintext and ciphertext
// outputs are never null, but an empty input may be, so inputs go through
// optionalSlice.
const aes_gcm = std.crypto.aead.aes_gcm;

fn optionalSlice(ptr: ?[*]const u8, len: usize) []const u8 {
    const p = ptr orelse {
        std.debug.assert(len == 0);
        return &.{};
    };
    return p[0..len];
}

export fn zig_crypto_aead_aes_gcm_encrypt(
    c: [*]u8,
    tag: *[aes_gcm.Aes256Gcm.tag_length]u8,
    m: ?[*]const u8,
    m_len: usize,
    ad: ?[*]const u8,
    ad_len: usize,
    nonce: *const [aes_gcm.Aes256Gcm.nonce_length]u8,
    key: [*]const u8,
    key_len: usize,
) callconv(.c) void {
    const out = c[0..m_len];
    const msg = optionalSlice(m, m_len);
    const assoc = optionalSlice(ad, ad_len);
    switch (key_len) {
        16 => aes_gcm.Aes128Gcm.encrypt(out, tag, msg, assoc, nonce.*, key[0..16].*),
        32 => aes_gcm.Aes256Gcm.encrypt(out, tag, msg, assoc, nonce.*, key[0..32].*),
        else => unreachable,
    }
}

// Returns false if the tag does not match, that is, the key, nonce, associated
// data, ciphertext or tag differs from what encryption used. The tag is
// checked before decrypting, so no unauthenticated plaintext is ever written
// to m.
export fn zig_crypto_aead_aes_gcm_decrypt(
    m: [*]u8,
    c: ?[*]const u8,
    c_len: usize,
    tag: *const [aes_gcm.Aes256Gcm.tag_length]u8,
    ad: ?[*]const u8,
    ad_len: usize,
    nonce: *const [aes_gcm.Aes256Gcm.nonce_length]u8,
    key: [*]const u8,
    key_len: usize,
) callconv(.c) bool {
    const out = m[0..c_len];
    const ct = optionalSlice(c, c_len);
    const assoc = optionalSlice(ad, ad_len);
    switch (key_len) {
        16 => aes_gcm.Aes128Gcm.decrypt(out, ct, tag.*, assoc, nonce.*, key[0..16].*) catch return false,
        32 => aes_gcm.Aes256Gcm.decrypt(out, ct, tag.*, assoc, nonce.*, key[0..32].*) catch return false,
        else => unreachable,
    }
    return true;
}

// Secure randomness behind env.random. Each thread has its own ChaCha8
// generator with forward secrecy, seeded on first use from the operating
// system through std.Io's randomSecure, which makes a syscall every time and
// has no weak fallback. The per-thread state avoids the global mutex that
// std.Io.random takes on threads that Zig did not start, which is all of ours.
// Returns false if the operating system could not provide a seed; the buffer
// is then left untouched and a later call tries to seed again.
const Csprng = std.Random.DefaultCsprng;
threadlocal var env_random_csprng: ?Csprng = null;

export fn zig_env_random_fill(buf: ?[*]u8, len: usize) callconv(.c) bool {
    if (len == 0) return true;
    if (env_random_csprng == null) {
        var seed: [Csprng.secret_seed_length]u8 = undefined;
        const io = std.Io.Threaded.global_single_threaded.io();
        io.randomSecure(&seed) catch return false;
        env_random_csprng = Csprng.init(seed);
        std.crypto.secureZero(u8, &seed);
    }
    env_random_csprng.?.fill(buf.?[0..len]);
    return true;
}

export fn zig_hash_wyhash_init(seed: u64) callconv(.c) *std.hash.Wyhash {
    // Wyhash state is only integers and bytes, so it need not be scanned
    const alloc = gc.atomicAllocator();
    const hasher_ptr = alloc.create(std.hash.Wyhash) catch {
        acton.raise_MemoryError("OOM while allocating Wyhash hasher");
        unreachable; // raise above does longjmp so this is unreachable
    };
    hasher_ptr.* = std.hash.Wyhash.init(seed);

    return hasher_ptr;
}

/// Size of the private Wyhash state used by the C runtime.  The ordinary
/// hasher constructor allocates this state on the GC heap because the
/// resulting Acton object may escape.  B_hash uses this size to reserve
/// suitably aligned automatic storage for its non-escaping hasher.
export fn zig_hash_wyhash_size() callconv(.c) usize {
    return @sizeOf(std.hash.Wyhash);
}

/// Initialize Wyhash in caller-owned storage.  The pointer must have the
/// alignment of std.hash.Wyhash; B_hash supplies max_align_t storage.
export fn zig_hash_wyhash_init_in_place(storage: *anyopaque, seed: u64) callconv(.c) *std.hash.Wyhash {
    const hasher_ptr: *std.hash.Wyhash = @ptrCast(@alignCast(storage));
    hasher_ptr.* = std.hash.Wyhash.init(seed);
    return hasher_ptr;
}

export fn zig_hash_wyhash_update(hasher: *std.hash.Wyhash, ptr: [*]const u8, len: usize) callconv(.c) void {
    hasher.update(ptr[0..len]);
}

export fn zig_hash_wyhash_total_len(hasher: *std.hash.Wyhash) callconv(.c) u64 {
    return hasher.total_len;
}

export fn zig_hash_wyhash_final(hasher: *std.hash.Wyhash) callconv(.c) u64 {
    return hasher.final();
}

export fn zig_hash_wyhash_hash_buffer(seed: u64, ptr: [*]const u8, len: usize) callconv(.c) u64 {
    return std.hash.Wyhash.hash(seed, ptr[0..len]);
}

export fn zig_hash_wyhash_hash(seed: u64, data: *acton.bytes) callconv(.c) u64 {
    const len: usize = @intCast(data.nbytes); // destination type from context
    return zig_hash_wyhash_hash_buffer(seed, data.str, len);
}
