/* bytesview.c */
    
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bytesview.h"

bytesview_t _empty = {
    ._pre = NULL,
    ._eof = 0,
    .buf = { ._start = 0, ._end = 0, ._bytes = (uint8_t *) ""}
};


act_bv_method_table_t *_bv_meth;

/* ### creation ### */

bytesview_t *act_bv_init (const uint8_t *b, size_t n, size_t start, const bytesview_t *pre, int closed)
{
    size_t len;
    bytesview_t *bv = (bytesview_t *) malloc(sizeof(bytesview_t));

    bv->_pre = (bytesview_t *) pre;              /* disable warning: discards const qualifier */
    bv->_eof = closed;
    if (start >= 0) {
        bv->buf._start = start;
    } else {
        bv->buf._start = 0;
    }
    if (n >= 0) {
        bv->buf._end = bv->buf._start+n;
    } else {
        len = strlen((char *) b);
        bv->buf._end = len - bv->buf._start;
    }
    bv->buf._bytes = (uint8_t *) b;              /* disable warning: discards const qualifier */
      
    return bv;
}

bytesview_t *act_bv_empty()
{
    return &_empty;
}

bytesview_t *act_bv_fromstr (const char *s)
{
    return act_bv_init((const uint8_t *)s, strlen(s), 0, NULL, 0);
}

bytesview_t *act_bv_frombuf(const bytesview_data_buffer_t *buf)
{
    bytesview_t *bv = (bytesview_t *) malloc(sizeof(bytesview_t));

    bv->_pre = NULL;
    bv->_eof = 0;
    bv->buf._start = buf->_start;
    bv->buf._end = buf->_end;
    bv->buf._bytes = buf->_bytes;

    return bv;
}

/* ### analysis ### */

size_t act_bv_n(const bytesview_t *self)
{
    size_t prelen;

    if (self->_pre == NULL) {
        prelen = 0;
    } else {
        prelen = act_bv_n(self->_pre);
    }
    return prelen + self->buf._end - self->buf._start;
}

static uint8_t *act_bv_cpybuf(const bytesview_t *self, uint8_t *p)
{
    /* copy all bytes to *p */

    uint8_t *q;
    size_t thislen;
    
    if (self->_pre != NULL) {
        q = act_bv_cpybuf(self->_pre, p);
    } else {
        q = p;
    }
    thislen = self->buf._end - self->buf._start;
    memcpy(q, self->buf._bytes+self->buf._start, thislen);
    return q+thislen;
}

bytesview_data_buffer_t *act_bv_tobuf(const bytesview_t *self)
{
    size_t n = act_bv_n(self);
    bytesview_data_buffer_t *p;
    uint8_t *q;

    if (self->buf._end - self->buf._start == n) {
        /* no more than one buffer */
        p = (bytesview_data_buffer_t *) &(self->buf);       /* disable warning: discards const qualifier */
    } else {
        /* several buffers, must concatenate */
        p = (bytesview_data_buffer_t *) malloc(sizeof(bytesview_data_buffer_t));
        p->_bytes = (uint8_t *) malloc (n+1);  /* room for NUL character at end */
        q = act_bv_cpybuf(self, p->_bytes);
        assert(q-p->_bytes == n);
        p->_start = 0;
        p->_end = n;
        *q = 0;
    }
    return p;
}

char *act_bv_tostr(const bytesview_t *self)
{
    bytesview_data_buffer_t *bp;

    bp = act_bv_tobuf(self);
    return (char *) bp->_bytes;
}


int act_bv_at_eof (const bytesview_t *self)
{
    return self->_eof;
}

/* ### de-lousing */
void act_bv_showbuf(const bytesview_data_buffer_t *buf)
{
    size_t i;
    size_t thislen;
    uint8_t *p;

    thislen = buf->_end - buf->_start;
    p = buf->_bytes;
    
    printf("[%lu,%lu] b'", buf->_start, buf->_end);
    i = 0;
    while (i++ < thislen) {
        printf("%c", *p++);
    }
    printf("'\n");
}

void act_bv_show(const bytesview_t *self)
{
    if (self == NULL) return;

    act_bv_show(self->_pre);
    printf("_eof=%d - buf= ", self->_eof);
    act_bv_showbuf(&self->buf);
}

void raise_IncompleteReadError(char *msg)
{
    fprintf(stderr, "IncompleteReadError: %s\n", msg);
    assert(0==1);
}
      
void raise_IndexError(char *msg)
{
    fprintf(stderr, "IndexError: %s\n", msg);
    assert(0==1);
}

void raise_ValueError(char *msg)
{
    fprintf(stderr, "ValueError: %s\n", msg);
    assert(0==1);
}

/* ### consuming data */

void act_bv__consume(const bytesview_t *self, int amount, bytesview_t **prep, size_t *remainingp)
{
    bytesview_t *pre, *newthis;
    size_t remaining, newremaining;
    size_t thislen;
    
    if (self->_pre != NULL) {
        act_bv__consume(self->_pre, amount, &pre, &remaining);     
    } else {
        pre = NULL;
        remaining = amount;
    }
    thislen = self->buf._end - self->buf._start;
    if (remaining < thislen) {
        newthis = act_bv_init(self->buf._bytes, thislen-remaining, self->buf._start+remaining, pre, self->_eof);
        newremaining = 0;
    } else {
        newthis = pre;
        newremaining = remaining - thislen;
    }
    *prep = newthis;
    *remainingp = newremaining; 
    return;
}

bytesview_t *act_bv_consume(const bytesview_t *self, size_t amount)
{
    bytesview_t *newview;
    size_t remaining;

    act_bv__consume(self, amount, &newview, &remaining);
    if (remaining > 0) {
        raise_IndexError("Not enough data to consume");
    }

    if (newview != NULL) {
        return newview;
    } else {
        return act_bv_empty();
    }
}


void act_bv__read(const bytesview_t *self, int n, int *remainingp, bytesview_t **initialp)
{
    bytesview_t *initial;
    int remaining;
    size_t thislen;

    assert(n>=0);

    if (self->_pre != NULL) {
        act_bv__read(self->_pre, n, &remaining, &initial);
    } else {
        remaining = n;
        initial = NULL;
    }
    thislen = self->buf._end-self->buf._start;
    if (remaining == 0) {
        *remainingp = remaining;
        *initialp = initial;
    } else if (remaining >= thislen) {
        *remainingp = remaining-thislen;
        *initialp = (bytesview_t *) self;
    } else {
        /*  0 < remaining < thislen */
        *remainingp = 0;
        *initialp = act_bv_init(self->buf._bytes, remaining, self->buf._start, initial, 0);
    }
    return;
}

bytesview_t *act_bv_read(const bytesview_t *self, size_t n)
{
    bytesview_t *initial;
    int remaining;
    int thislen;
      
    if (n == -1) {
        if (!self->_eof) {
            raise_IncompleteReadError("read(-1) on non-closed Bytesview");
            return NULL;
         } else {
            return (bytesview_t *) self;
         }
    } else {
         /* deliver n bytes, if there are n bytes.
          * if not, deliver available bytes if closed, error if not closed
          */
        thislen = self->buf._end - self->buf._start;
        if (self->_pre != NULL) {
            act_bv__read(self->_pre, n, &remaining, &initial);
        } else {
            remaining = n;
            initial = NULL;
        }

        if (remaining == 0) {
            if (initial != NULL) {
                return initial;
            } else {
                return act_bv_empty();
            }
        } else if (remaining < thislen) {
            return act_bv_init(self->buf._bytes, remaining, self->buf._start, initial, 0);
        } else if (remaining > thislen) {
          if (self->_eof) return (bytesview_t *) self;
            else raise_IncompleteReadError("read(n) on non-closed Bytesview with less than n bytes");
            return NULL;
        } else {
            /* remaining == thislen */
          return (bytesview_t *) self;
        }
    } 
}

bytesview_t *act_bv_readline(const bytesview_t *self)
{
    return act_bv_readuntil(self, "\n");
}

static void act_bv__readexactly(const bytesview_t *self, size_t n, bytesview_t **initialp, size_t *remainsp)
{
    bytesview_t *initial;
    size_t remains, thislen;

    thislen = self->buf._end - self->buf._start;
    if (self->_pre != NULL) {
        act_bv__readexactly(self->_pre, n, &initial, &remains);
    } else {
        initial = NULL;
        remains = n;
    }

    if (remains == 0) {
        *initialp = initial;
        *remainsp = remains;
    } else if (remains >= thislen) {
        *initialp = (bytesview_t *) self;
        *remainsp = remains - thislen;
    } else {
        /* remains < thislen */
        *initialp = act_bv_init(self->buf._bytes, remains, self->buf._start, initial, 0);
        *remainsp = 0;
    }
}

bytesview_t *act_bv_readexactly(const bytesview_t *self, size_t n) {
    bytesview_t *initial;
    size_t remains;

    act_bv__readexactly(self, n, &initial, &remains);
    if (remains > 0) {
        raise_IncompleteReadError("readexactly(): Too few bytes available");
        return NULL;
    }

    if (initial == NULL) {
        initial = act_bv_empty();
    }
    
    return initial;
}

static int act_bv__readuntil(const bytesview_t *self, void const *separator, size_t seplen,
    bytesview_data_buffer_t *last,
    bytesview_t **initialp)
{
    bytesview_t *initial;
    int found;
    uint8_t *buf;

    /* len(last) < len(separator) */

    if (self->_pre != NULL) {
        found = act_bv__readuntil(self->_pre, separator, seplen, &last, &initial)
    } else {
        last = "";
        initial = NULL;
        found = 0;
    }

    // last is potential beginning of separator at end of previous Bytesview(s)

    if (found) {
        *lastp = "";
        *initialp = initial;
        return found;
    } else {
        XXXXX
    }
}

bytesview_t *act_bv_readuntil(const bytesview_t *self, void const *separator)  {
    bytesview_data_buffer_t buf, *last;
    size_t seplen;
    bytesview_t *initial;
    int found;

    seplen = strlen(separator);
    last = &buf;
    last->_start = 0;
    last->_end = 0;
    last->_bytes = (uint8_t *) malloc (seplen + seplen -1);   // room for (seplen-1)*2 bytes + nul byte

    found = act_bv__readuntil(self, separator, seplen, last, &initial);

    if (found) {
        if (initial != NULL) {
            return initial;
        } else {
            return act_bv_empty();
        }
    } else {
        if (self->_eof) {
            raise_IncompleteReadError("Separator not found and Bytesview closed");
            return NULL;
        } else {
            raise_IncompleteReadError("Separator not found");
            return NULL;
        }
    }
}

/* ### feeding data & closing */

bytesview_t *act_bv_append(const bytesview_t *self, void const *b, size_t n)  {
    if (self->_eof) {
        raise_ValueError("append() on closed Bytesview");
    }
    if (self->buf._start < self->buf._end) {
        return act_bv_init(b, n, 0, self, 0);
    } else {
        return act_bv_init(b, n, 0, NULL, 0);
    }
}

bytesview_t *act_bv_write(const bytesview_t *self, void const *b, size_t n)  {
    if (self->_eof) {
        raise_ValueError("write() on closed Bytesview");
    } 
    return act_bv_append(self, b, n);
}

bytesview_t *act_bv_writelines(const bytesview_t *self, void const **data)  {
    if (self->_eof) {
        raise_ValueError("writelines() on closed Bytesview");
    }

    /* XXX */
    assert(0);
    return act_bv_empty();
    
}

bytesview_t *act_bv_close (const bytesview_t *self)  {
    if (self->_eof) return (bytesview_t *) self;             /* disable warning: discards const qualifier */
    return act_bv_init(self->buf._bytes, self->buf._end-self->buf._start, self->buf._start, self->_pre, 1);
}


/* method table */

act_bv_method_table_t _act_bv_method_table = {
    .init = act_bv_init,
    .empty = act_bv_empty
};

void act_bv_class_init() {
    _bv_meth = & _act_bv_method_table;
}

