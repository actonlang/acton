
#include "../rts/io.h"
#include "../rts/log.h"
#include <snappy-c.h>

void snappyQ___ext_init__() {
    // NOP
}

B_bytes snappyQ_compress (B_bytes data) {
    char *input;
    char *compressed;
    size_t input_len;
    size_t compressed_len;
    snappy_status status;
    B_bytes ret;

    input = (char*)fromB_bytes(data);
    input_len = (size_t)data->nbytes;

    compressed_len = snappy_max_compressed_length(input_len);
    compressed = acton_malloc(compressed_len);
    status = snappy_compress(input, input_len, compressed, &compressed_len);

    if (SNAPPY_OK == status) {
        ret = to$bytesD_len(compressed, (int)compressed_len);
    }
    else {
        char *errmsg = SNAPPY_INVALID_INPUT == status ? "Invalid input" : "Buffer too small";
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str(errmsg)));
    }

    return ret;
}

B_bytes snappyQ_decompress (B_bytes data) {
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
	char *errmsg = (SNAPPY_INVALID_INPUT == status) ? "Invalid input" : "Buffer too small";
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str(errmsg)));
    }

    uncompressed = acton_malloc(uncompressed_len);
    snappy_uncompress(input, input_len, uncompressed, &uncompressed_len);

    if (SNAPPY_OK == status) {
	ret = to$bytesD_len(uncompressed, (int)uncompressed_len);
    }
    else {
	char *errmsg = SNAPPY_INVALID_INPUT == status ? "Invalid input" : "Buffer too small";
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str(errmsg)));
    }

    return ret;
}
