
#include "rts/io.h"
#include "rts/log.h"
#include <snappy-c.h>

static struct B_ValueError stdQ_snappyQ_invalid_input_error =
    STATIC_EXCEPTION(B_ValueError, "Invalid input");
static struct B_ValueError stdQ_snappyQ_small_buffer_error =
    STATIC_EXCEPTION(B_ValueError, "Buffer too small");

void stdQ_snappyQ___ext_init__() {
    // NOP
}

B_bytes stdQ_snappyQ_compress (B_bytes data) {
    char *input;
    char *compressed;
    size_t input_len;
    size_t compressed_len;
    snappy_status status;
    B_bytes ret;

    input = (char*)fromB_bytes(data);
    input_len = (size_t)data->nbytes;

    compressed_len = snappy_max_compressed_length(input_len);
    compressed = acton_malloc_atomic(compressed_len);
    status = snappy_compress(input, input_len, compressed, &compressed_len);

    if (SNAPPY_OK == status) {
        ret = actBytesFromCStringLengthCopy(compressed, (int)compressed_len);
    }
    else {
        RAISE_EXC(SNAPPY_INVALID_INPUT == status
            ? &stdQ_snappyQ_invalid_input_error
            : &stdQ_snappyQ_small_buffer_error);
    }

    return ret;
}

B_bytes stdQ_snappyQ_decompress (B_bytes data) {
    char *input;
    char *uncompressed;
    size_t input_len;
    size_t uncompressed_len;
    snappy_status status;
    B_bytes ret;

    input = (char*)fromB_bytes(data);
    input_len = (size_t)data->nbytes;

    status = snappy_uncompressed_length(input, input_len, &uncompressed_len);

    if (SNAPPY_OK != status) {
        RAISE_EXC(SNAPPY_INVALID_INPUT == status
            ? &stdQ_snappyQ_invalid_input_error
            : &stdQ_snappyQ_small_buffer_error);
    }

    // Not zeroed, so a failed uncompress must not return the buffer
    uncompressed = acton_malloc_atomic(uncompressed_len);
    status = snappy_uncompress(input, input_len, uncompressed, &uncompressed_len);

    if (SNAPPY_OK == status) {
	ret = actBytesFromCStringLengthCopy(uncompressed, (int)uncompressed_len);
    }
    else {
        RAISE_EXC(SNAPPY_INVALID_INPUT == status
            ? &stdQ_snappyQ_invalid_input_error
            : &stdQ_snappyQ_small_buffer_error);
    }

    return ret;
}
