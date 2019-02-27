/* C version of bytesview
 */

#include <sys/types.h>

#define DEFAULT_IO_BUFFER_SIZE 65536

typedef struct {
    size_t _begin
    size_t _end
    uint8_t _bytes[DEFAULT_IO_BUFFER_SIZE]
} bytesview_data_buffer_t;

typedef struct {
    bytesview_t *_pre;
    int _eof;
    bytesview_data_buffer_t *buf;
} bytesview_t;

bytesview_t *act_io_bv_init(bytesview_t *self, uint8_t *b, size_t n, size_t start, bytesview_t *pre, int closed);

bytesview_t *act_io_bv_empty();

bytesview_t *act_io_bv_from_buf(bytesview_data_buffer_t *buf);

bytesview_t *act_io_bv_n(bytesview_t *self);

char *act_io_bv_to_bytes(bytesview_t *self);

bytesview_t *act_io_bv_show(bytewview_t *self);

bytesview_t *act_io_bv_consume(bytesview_t *self, size_t amount);

bytesview_t *act_io_bv_read(bytesview_t *self, size_t n);

bytesview_t *act_io_bv_readline(bytesview_t *self);

bytesview_t *act_io_bv_readexactly(bytesview_t *self, size_t n);

bytesview_t *act_io_bv_readuntil(bytesview_t *self, void const *separator);

int act_io_bv_at_eof (bytesview_t *self, ...);

bytesview_t *act_io_bv_append(bytesview_t *self, void const *b, size_t n);

bytesview_t *act_io_bv_write(bytesview_t *self, void const *b, size_t n);

bytesview_t *act_io_bv_writelines(bytesview_t *self, void const **data);

bytesview_t *act_io_bv_close (bytesview_t *self);

struct act_io_bv_methods {
  bytesview init 

