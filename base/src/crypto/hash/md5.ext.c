void cryptoQ_hashQ_md5Q___ext_init__() {}

void *zig_crypto_hash_md5_init();
void *zig_crypto_hash_md5_update(void *hasher, B_bytes data);
void *zig_crypto_hash_md5_finalize(void *hasher, B_bytes output);

B_NoneType cryptoQ_hashQ_md5Q_HasherD__init (cryptoQ_hashQ_md5Q_Hasher self) {
    self->_hasher = zig_crypto_hash_md5_init();
    return B_None;
}
B_NoneType cryptoQ_hashQ_md5Q_HasherD_update (cryptoQ_hashQ_md5Q_Hasher self, B_bytes data) {
    zig_crypto_hash_md5_update(self->_hasher, data);
    return B_None;
}
B_bytes cryptoQ_hashQ_md5Q_HasherD_finalize (cryptoQ_hashQ_md5Q_Hasher self) {
    B_bytes output = to$bytes("1234567890abcdef");
    zig_crypto_hash_md5_finalize(self->_hasher, output);
    return output;
}
