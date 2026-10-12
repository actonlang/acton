#include <stdio.h>

void X_32_Q___ext_init__() {}

B_NoneType X_32_Q_write_stdout(B_bytes data) {
    fwrite(data->str, 1, data->nbytes, stdout);
    return B_None;
}
