/* bytesview.h */

/* C version of bytesview
 */

#include <stdint.h>
#include <sys/types.h>

#define ACT_BV_DEFAULT_BUFFER_SIZE 8192

typedef struct {
    size_t _start;
    size_t _end;
    uint8_t *_bytes;         /* will usually have size ACT_BV_DEFAULT_BUFFER_SIZE */
} bytesview_data_buffer_t;

typedef struct bytesview_t {
    struct bytesview_t *_pre;
    int _eof;
    bytesview_data_buffer_t buf;
} bytesview_t;

extern void act_bv_class_init();

/* ### Creation ### */

bytesview_t const *act_bv_init(uint8_t const *b, size_t n, size_t start, const bytesview_t *pre, int closed);

bytesview_t const *act_bv_empty();

bytesview_t const *act_bv_frombuf(bytesview_data_buffer_t const *buf);

bytesview_t const *act_bv_fromstr(char const *s);

/* ### Analysis ### */

size_t act_bv_n(bytesview_t const *self);

bytesview_data_buffer_t const *act_bv_tobuf(bytesview_t const *self);   /* return value SHOULD NOT be mutated */

char *act_bv_tostr(bytesview_t const *self);   /* return value SHOULD NOT be mutated */

int act_bv_at_eof (bytesview_t const *self);

/* ### De-lousing ### */

void act_bv_show(bytesview_t const *self);

void act_bv_showbuf(bytesview_data_buffer_t const *buf);

void raise_IncompleteReadError(char *msg);

void raise_IndexError(char *msg);

void raise_ValueError(char *msg);

/* ### Consuming data ### */

bytesview_t const *act_bv_consume(bytesview_t const *self, size_t amount);

bytesview_t const *act_bv_read(bytesview_t const *self, size_t n);

bytesview_t const *act_bv_readline(bytesview_t const *self);

bytesview_t const *act_bv_readexactly(bytesview_t const *self, size_t n);

bytesview_t const *act_bv_readuntil(bytesview_t const *self, void const *separator);

/* ### Adding data ### */

bytesview_t const *act_bv_append(bytesview_t const *self, void const *b, size_t n);

bytesview_t const *act_bv_write(bytesview_t const *self, void const *b, size_t n);

bytesview_t const *act_bv_writelines(bytesview_t const *self, void const **data);

bytesview_t const *act_bv_close (bytesview_t const *self);


typedef int (*BV_INT_T)(int);
typedef bytesview_t const * (*BYTESVIEW_INIT_T)(const uint8_t *b, size_t n, size_t start, bytesview_t const *pre, int closed);
typedef bytesview_t const * (*BYTESVIEW_EMPTY_T)(bytesview_t const *self);
typedef bytesview_t const * (*BYTESVIEW_FROMBUF_T)(bytesview_data_buffer_t const *buf);
typedef bytesview_t const * (*BYTESVIEW_FROMSTR_T)(char const *s);



typedef struct {
  BV_INT_T intfunc;
  BYTESVIEW_INIT_T init;    		/*  = act_bv_init; */
  BYTESVIEW_EMPTY_T empty;    		/*  = act_bv_empty; */
  BYTESVIEW_FROMBUF_T frombuf;    	/*  = act_bv_frombuf; */

} act_bv_method_table_t;


