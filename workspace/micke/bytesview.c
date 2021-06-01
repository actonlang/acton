/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* bytesview.c */
    
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bytesview.h"

bytesview_t const _empty = {
    ._pre = NULL,
    ._eof = 0,
    .buf = { ._start = 0, ._end = 0, ._bytes = (uint8_t *) ""}
};


act_bv_method_table_t *_bv_meth;

/* ### creation ### */

bytesview_t const *act_bv_init (const uint8_t *b, size_t n, size_t start, const bytesview_t *pre, int closed)
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

bytesview_t const *act_bv_empty()
{
    return &_empty;
}

bytesview_t const *act_bv_fromstr (char const *s)
{
    return act_bv_init((uint8_t const *)s, strlen(s), 0, NULL, 0);
}

bytesview_t const *act_bv_frombuf(bytesview_data_buffer_t const *buf)
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

size_t act_bv_n(bytesview_t const *self)
{
    size_t prelen;

    if (self->_pre == NULL) {
        prelen = 0;
    } else {
        prelen = act_bv_n(self->_pre);
    }
    return prelen + self->buf._end - self->buf._start;
}

static uint8_t *act_bv_cpybuf(bytesview_t const *self, uint8_t *p)
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

bytesview_data_buffer_t const *act_bv_tobuf(bytesview_t const *self)
{
    size_t n = act_bv_n(self);
    bytesview_data_buffer_t *p;

#if NDEBUG
#else
    uint8_t *q;
#endif

    if (self->buf._end - self->buf._start == n) {
        /* no more than one buffer */
        p = (bytesview_data_buffer_t *) &(self->buf);
    } else {
        /* several buffers, must concatenate */
        p = (bytesview_data_buffer_t *) malloc(sizeof(bytesview_data_buffer_t));
        p->_bytes = (uint8_t *) malloc (n);
#if NDEBUG
        act_bv_cpybuf(self, p->_bytes);
#else
        q  = act_bv_cpybuf(self, p->_bytes);
        assert(q-p->_bytes == n);
#endif		      
        p->_start = 0;
        p->_end = n;
    }
    return p;
}

char *act_bv_tostr(bytesview_t const *self)
{
    size_t n = act_bv_n(self);
    uint8_t *p, *q;
    
    p = (uint8_t *) malloc(n+1);  /* room for NUL character at end */
    q = act_bv_cpybuf(self, p);
    assert(q-p == n);
    *q = 0;    /* NUL character at end */
    return (char *) p;
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
    p = buf->_bytes+buf->_start;
    
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
    // assert(0==1);
}
      
void raise_IndexError(char *msg)
{
    fprintf(stderr, "IndexError: %s\n", msg);
    // assert(0==1);
}

void raise_ValueError(char *msg)
{
    fprintf(stderr, "ValueError: %s\n", msg);
    // assert(0==1);
}

/* ### consuming data */

void act_bv__consume(bytesview_t const *self, int amount, bytesview_t const **prep, size_t *remainingp)
{
    bytesview_t const *pre;
    bytesview_t const *newthis;
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

bytesview_t const *act_bv_consume(bytesview_t const *self, size_t amount)
{
    bytesview_t const *newview;
    size_t remaining;

    act_bv__consume(self, amount, &newview, &remaining);
    if (remaining > 0) {
        raise_IndexError("Not enough data to consume");
        return NULL;
    }

    if (newview != NULL) {
        return newview;
    } else {
        return act_bv_empty();
    }
}


void act_bv__read(bytesview_t const *self, int n, int *remainingp, bytesview_t const **initialp)
{
    bytesview_t const *initial;
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
        *initialp = self;
    } else {
        /*  0 < remaining < thislen */
        *remainingp = 0;
        *initialp = act_bv_init(self->buf._bytes, remaining, self->buf._start, initial, 0);
    }
    return;
}

bytesview_t const *act_bv_read(bytesview_t const *self, size_t n)
{
    bytesview_t const *initial;
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

bytesview_t const *act_bv_readline(bytesview_t const *self)
{
    return act_bv_readuntil(self, "\n");
}

static void act_bv__readexactly(bytesview_t const *self, size_t n, bytesview_t const **initialp, size_t *remainsp)
{
    bytesview_t const *initial;
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

bytesview_t const *act_bv_readexactly(const bytesview_t *self, size_t n) {
    bytesview_t const *initial;
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

static size_t act_bv_min(size_t a, size_t b)
{
    if (a <= b) return a;
    else return b;
}

static int act_bv__readuntil(bytesview_t const *self, void const *separator, size_t seplen,
    bytesview_data_buffer_t *last,
    bytesview_t const ** initialp)
{
    bytesview_t const *initial;
    int found;
    uint8_t *m;
    size_t idx, lastlen, cpylen, thislen, keeplen;

    if (self->_pre != NULL) {
        found = act_bv__readuntil(self->_pre, separator, seplen, last, &initial);
    } else {
        /* last initialized in act_bv_readuntil() */
        initial = NULL;
        found = 0;
        //printf("-");	
    }
    // last is potential beginning of separator at end of previous buffer(s)

    if (found) {
        /* last made empty in recursive call */
        *initialp = initial;
        return found;
    }
    //printf("-\n");
    
    /* still looking - is separator straddling buffers? */
    lastlen = last->_end - last->_start;
    thislen = self->buf._end - self->buf._start;
    cpylen = act_bv_min(seplen-1, thislen);
    memcpy(last->_bytes+last->_end, self->buf._bytes+self->buf._start, cpylen);
    lastlen = last->_end += cpylen;
    /* now, last contains last part of previous plus first part of this */
    //printf("lastlen = %ld, cpylen = %ld\n", lastlen, cpylen);
    //printf("last: ");
    //act_bv_showbuf(last);
    if (lastlen >= seplen) {
        // might be straddling, check!
        m = (uint8_t *) strstr((const char *)last->_bytes+last->_start, separator);
        if (m != NULL) {
            found = 1;
	    idx = m - (last->_bytes+last->_start);
            //printf("straddling! idx=%ld cpylen=%ld last: [%ld, %ld]\n", idx, cpylen, last->_start, last->_end);
	    // end of separator at idx + seplen - len(last)
	    *initialp = act_bv_init(self->buf._bytes, cpylen-(last->_end-(idx+seplen)), self->buf._start, self->_pre, 0); 
	    // last->_start = last->_end = 0;
	    return found;
	}
    }
    
    /* Nope, so search this buffer */
    m = memmem(self->buf._bytes+self->buf._start, self->buf._end-self->buf._start, separator, seplen);
    if (m != NULL) {
	found = 1;
	idx = m - (self->buf._bytes + self->buf._start);
        //printf("inbuf [%ld, %ld]: idx == %ld\n", self->buf._start, self->buf._end, idx);
	*initialp = act_bv_init(self->buf._bytes, idx+seplen, self->buf._start, self->_pre, 0);
	return found;
    }
    
    /* Nope, so return last part of this buffer in case separator is straddling */
    found = 0;
    if (thislen >= seplen-1) {
	/* discard old contents of last */
	last->_start = 0;
	last->_end = seplen-1;
	memcpy(last->_bytes, self->buf._bytes+self->buf._end-(seplen-1), seplen-1);
	*(last->_bytes+last->_end) = 0;
    } else {
        /* entire this is in last, so just keep end of last */
        keeplen = act_bv_min(seplen-1, lastlen);
	if (keeplen < lastlen) {
	    // move end of last up front  
	    memcpy(last->_bytes, last->_bytes+last->_end-keeplen, keeplen);
	    last->_start = 0;
	    last->_end = keeplen;
 	    *(last->_bytes+last->_end) = 0;
	} else {
	    /* keep all of last */
	}
    }
    *initialp = self;
    return found;
}

bytesview_t const *act_bv_readuntil(bytesview_t const *self, void const *separator)
{
    bytesview_data_buffer_t buf, *last;
    size_t seplen;
    bytesview_t const *initial;
    int found;

    seplen = strlen(separator);
    last = &buf;
    last->_start = 0;
    last->_end = 0;
    last->_bytes = (uint8_t *) malloc (seplen + seplen -1);   // room for (seplen-1)*2 bytes + nul byte
    *(last->_bytes) = 0;
    
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

bytesview_t const *act_bv_append(bytesview_t const *self, void const *b, size_t n)
{
    if (self->_eof) {
        raise_ValueError("append() on closed Bytesview");
	return NULL;
    }
    if (self->buf._start < self->buf._end) {
        return act_bv_init(b, n, 0, self, 0);
    } else {
        return act_bv_init(b, n, 0, NULL, 0);
    }
}

bytesview_t const *act_bv_write(bytesview_t const *self, void const *b, size_t n)
{
    if (self->_eof) {
        raise_ValueError("write() on closed Bytesview");
	return NULL;
    } 
    return act_bv_append(self, b, n);
}

bytesview_t const *act_bv_writelines(bytesview_t const *self, void const **data)
{
    if (self->_eof) {
        raise_ValueError("writelines() on closed Bytesview");
    }

    /* XXX */
    assert(0);
    return act_bv_empty();
}

bytesview_t const *act_bv_close (bytesview_t const *self)
{
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

