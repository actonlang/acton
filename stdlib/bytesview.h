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

bytesview_t *act_bv_init(const uint8_t *b, size_t n, size_t start, const bytesview_t *pre, int closed);

bytesview_t *act_bv_empty();

bytesview_t *act_bv_frombuf(const bytesview_data_buffer_t *buf);

bytesview_t *act_bv_fromstr(const char *s);

/* ### Analysis ### */

size_t act_bv_n(const bytesview_t *self);

bytesview_data_buffer_t *act_bv_tobuf(const bytesview_t *self);   /* return value SHOULD NOT be mutated */

char *act_bv_tostr(const bytesview_t *self);   /* return value SHOULD NOT be mutated */

int act_bv_at_eof (const bytesview_t *self);

/* ### De-lousing ### */

void act_bv_show(const bytesview_t *self);

void act_bv_showbuf(const bytesview_data_buffer_t *buf);

void raise_IncompleteReadError(char *msg);

void raise_IndexError(char *msg);

void raise_ValueError(char *msg);

/* ### Consuming data ### */

bytesview_t *act_bv_consume(const bytesview_t *self, size_t amount);

bytesview_t *act_bv_read(const bytesview_t *self, size_t n);

bytesview_t *act_bv_readline(const bytesview_t *self);

bytesview_t *act_bv_readexactly(const bytesview_t *self, size_t n);

bytesview_t *act_bv_readuntil(const bytesview_t *self, void const *separator);

/* ### Adding data ### */

bytesview_t *act_bv_append(const bytesview_t *self, void const *b, size_t n);

bytesview_t *act_bv_write(const bytesview_t *self, void const *b, size_t n);

bytesview_t *act_bv_writelines(const bytesview_t *self, void const **data);

bytesview_t *act_bv_close (const bytesview_t *self);


typedef int (*BV_INT_T)(int);
typedef bytesview_t * (*BYTESVIEW_INIT_T)(const uint8_t *b, size_t n, size_t start, const bytesview_t *pre, int closed);
typedef bytesview_t * (*BYTESVIEW_EMPTY_T)(const bytesview_t *self);
typedef bytesview_t * (*BYTESVIEW_FROMBUF_T)(const bytesview_data_buffer_t *buf);
typedef bytesview_t * (*BYTESVIEW_FROMSTR_T)(const char *s);



typedef struct {
  BV_INT_T intfunc;
  BYTESVIEW_INIT_T init;      		/*  = act_bv_init; */
  BYTESVIEW_EMPTY_T empty;    		/*  = act_bv_empty; */
  BYTESVIEW_FROMBUF_T frombuf;    	/*  = act_bv_frombuf; */

} act_bv_method_table_t;


