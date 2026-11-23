// AES-GCM itself comes from Zig's std.crypto, see base/__root.zig. This file
// checks arguments, lays out nonce | ciphertext | tag and raises exceptions.

void zig_crypto_aead_aes_gcm_encrypt(unsigned char *c, unsigned char *tag,
                                     const unsigned char *m, size_t m_len,
                                     const unsigned char *ad, size_t ad_len,
                                     const unsigned char *nonce,
                                     const unsigned char *key, size_t key_len);
bool zig_crypto_aead_aes_gcm_decrypt(unsigned char *m,
                                     const unsigned char *c, size_t c_len,
                                     const unsigned char *tag,
                                     const unsigned char *ad, size_t ad_len,
                                     const unsigned char *nonce,
                                     const unsigned char *key, size_t key_len);

#define AES_GCM_KEY_SIZE 32
#define AES_GCM_NONCE_SIZE 12
#define AES_GCM_TAG_SIZE 16
// GCM's 32-bit block counter allows at most 2^32 - 2 blocks of 16 bytes under
// one nonce.
#define AES_GCM_MAX_TEXT_SIZE (16 * ((INT64_C(1) << 32) - 2))

void stdQ_cryptoQ_aeadQ_aes_gcmQ___ext_init__() {}

static void aes_gcm_check_key(B_bytes key) {
    if (key->nbytes != 16 && key->nbytes != 32)
        RAISE(B_ValueError, $FORMAT("AES-GCM key must be 16 or 32 bytes, got %lld", (long long)key->nbytes));
}

// The buffer holds no pointers, so it is allocated atomic. It is never empty,
// since the Zig functions take the output pointers as non-null.
static unsigned char *aes_gcm_alloc(int64_t len) {
    unsigned char *buf = acton_malloc_atomic(len > 0 ? (size_t)len : 1);
    if (buf == NULL)
        RAISE(B_MemoryError, actStrFromCString("Out of memory in AES-GCM"));
    return buf;
}

// generate_key and encrypt take the EnvRandom only as the capability to draw
// random numbers. They fill keys and nonces from its generator directly rather
// than call its methods, which a subclass could override.
B_bytes stdQ_cryptoQ_aeadQ_aes_gcmQ_generate_key(B_EnvRandom rand) {
    unsigned char *key = aes_gcm_alloc(AES_GCM_KEY_SIZE);
    B_EnvRandom_fill(key, AES_GCM_KEY_SIZE);
    return actBytesFromCStringLength((const char *)key, AES_GCM_KEY_SIZE);
}

B_bytes stdQ_cryptoQ_aeadQ_aes_gcmQ_encrypt(B_EnvRandom rand, B_bytes key, B_bytes plaintext, B_bytes associated_data) {
    aes_gcm_check_key(key);
    int64_t text_len = plaintext->nbytes;
    if (text_len > AES_GCM_MAX_TEXT_SIZE)
        RAISE(B_ValueError, actStrFromCString("AES-GCM plaintext must be at most 64 GiB"));

    int64_t out_len = AES_GCM_NONCE_SIZE + text_len + AES_GCM_TAG_SIZE;
    unsigned char *out = aes_gcm_alloc(out_len);
    unsigned char *nonce = out;
    unsigned char *c = out + AES_GCM_NONCE_SIZE;
    unsigned char *tag = c + text_len;
    B_EnvRandom_fill(nonce, AES_GCM_NONCE_SIZE);
    zig_crypto_aead_aes_gcm_encrypt(c, tag,
                                    plaintext->str, (size_t)text_len,
                                    associated_data->str, (size_t)associated_data->nbytes,
                                    nonce, key->str, (size_t)key->nbytes);
    return actBytesFromCStringLength((const char *)out, out_len);
}

B_bytes stdQ_cryptoQ_aeadQ_aes_gcmQ_decrypt(B_bytes key, B_bytes ciphertext, B_bytes associated_data) {
    aes_gcm_check_key(key);
    // A ciphertext too short to hold a nonce and a tag, or too long for GCM,
    // cannot have come from encrypt.
    int64_t text_len = ciphertext->nbytes - AES_GCM_NONCE_SIZE - AES_GCM_TAG_SIZE;
    if (text_len < 0 || text_len > AES_GCM_MAX_TEXT_SIZE)
        RAISE(stdQ_cryptoQ_aeadQ_aes_gcmQ_DecryptionError, actStrFromCString("AES-GCM ciphertext has invalid length"));

    const unsigned char *nonce = ciphertext->str;
    const unsigned char *c = nonce + AES_GCM_NONCE_SIZE;
    const unsigned char *tag = c + text_len;
    unsigned char *out = aes_gcm_alloc(text_len);
    if (!zig_crypto_aead_aes_gcm_decrypt(out, c, (size_t)text_len, tag,
                                         associated_data->str, (size_t)associated_data->nbytes,
                                         nonce, key->str, (size_t)key->nbytes))
        RAISE(stdQ_cryptoQ_aeadQ_aes_gcmQ_DecryptionError, actStrFromCString("AES-GCM authentication failed"));
    return actBytesFromCStringLength((const char *)out, text_len);
}
