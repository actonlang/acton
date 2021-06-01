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

/* bytesview_test.c */
    
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bytesview.h"

void report(bytesview_t const *bv, char *name)
{
    
    printf("%s.n() = %ld\n", name, act_bv_n(bv));
    printf("%s.tostr() = %s\n", name, act_bv_tostr(bv));
    printf("%s.show():\n", name);
    act_bv_show(bv);
    printf("\n");
}


void consumetest(bytesview_t const *bv, int additional, int show)
{  
    bytesview_t const *bvr;

    size_t i, iterations;

    printf("consume test (show=%d)\n", show);
    printf("=====================\n");
    printf("bv: b'%s'\n", act_bv_tostr(bv));
    printf("additional: %d\n", additional);
    printf("---\n");

    iterations = act_bv_n(bv) + 2;
    i = 0;
    while (i < iterations) {
        bvr = act_bv_consume(bv, i);
	if (additional>0 && bvr != NULL) bvr = act_bv_consume(bvr, additional);
        if (show) {
	    if (bvr != NULL) report(bvr,"bvr");
	    else printf("NULL\n");
        } else {
	    if (bvr != NULL) printf("bvr: b'%s'\n", act_bv_tostr(bvr));
	    else printf("NULL\n");
	}
        i++;
    }
}

void readtest(bytesview_t const *bv, int show)
{  
    bytesview_t const *bvr;

    size_t i, iterations;

    printf("read test (show = %d)\n", show);
    printf("====================\n");
    iterations = act_bv_n(bv) + 2;
    // iterations = 3;
    i = 0;
    while (i < iterations) {
        bvr = act_bv_read(bv, i);
        if (show) {
            if (bvr != NULL) report(bvr,"bv");
            else printf("NULL\n");
	} else {
	    if (bvr != NULL) printf("b'%s'\n", act_bv_tostr(bvr));
	    else printf("NULL\n");
	}	  
        i++;
    }
}

void readexactlytest(bytesview_t const *bv, int show)
{  
    bytesview_t const *bvr;

    size_t i, iterations;

    printf("readexactly test (show = %d)\n", show);
    printf("===========================\n");
    iterations = act_bv_n(bv) + 2;
    // iterations = 3;
    i = 0;
    while (i < iterations) {
        bvr = act_bv_readexactly(bv, i);
        if (show) {
            if (bvr != NULL) report(bvr,"bv");
            else printf("NULL\n");
	} else {
	    if (bvr != NULL) printf("b'%s'\n", act_bv_tostr(bvr));
	    else printf("NULL\n");
	}	  
        i++;
    }
}

void readuntiltest(bytesview_t const *bv, char *sep, int show)
{  
    bytesview_t const *bvr;
    bytesview_t const *bvs;

    size_t seplen = strlen(sep);

    size_t i, iterations;

    printf("readuntil test (show = %d)\n", show);
    printf("=========================\n");
    printf("data = '%s'\n", act_bv_tostr(bv));
    printf("separator ='%s'\n", sep);
    printf("seplen = %ld\n", seplen);
    printf("---\n");
    
    iterations = act_bv_n(bv) + 2;

    i = 0;
    while (i < iterations) {
        bvr = act_bv_consume(bv, i);
        if (bvr != NULL) {
	    bvs = act_bv_readuntil(bvr, sep);
	    if (show) {
		if (bvr != NULL) printf("bvr: b'%s'\n", act_bv_tostr(bvr));
		else printf("bvr: NULL\n");
		if (bvs != NULL) report(bvs,"bvs");
		else printf("bvs: NULL\n");
	    } else {
		if (bvr != NULL) printf("b'%s'\n", act_bv_tostr(bvr));
		else printf("NULL\n");
		if (bvs != NULL) printf("b'%s'\n", act_bv_tostr(bvs));
		else printf("NULL\n");		
	    }
	}
        i++;
    }
}

int main() {

    char *by = "0123456789abcdef";

    bytesview_data_buffer_t b = { ._start = 0, ._end = 16, ._bytes = (uint8_t *) by };

    size_t l = strlen(by);
    //int j;
    
    bytesview_t const *bv1;
    bytesview_t const *bv2;
    bytesview_t const *bv3;
    bytesview_t const *bv4;
    bytesview_t const *bv5;
    
    bv1 = act_bv_fromstr(by);
    report(bv1, "bv1");

    bv2 = act_bv_frombuf(&b);
    report(bv2, "bv2");

    bv3 = act_bv_append(bv2, by, l);
    report(bv3, "bv3");
    
    bv4 = act_bv_append(bv1, "x", 1);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, "z", 1);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, "w", 1);
    report(bv4, "bv4");

    bv5 = act_bv_close(bv4);
    report(bv5, "bv5");
    
    //consumetest(bv4, 20, 1);
    
    //consumetest(bv5);
    
    //readtest(bv4,1);
    //readtest(bv4,0);

    //readtest(bv5,1);
    //readtest(bv5,0);

    //readexactlytest(bv4,1);
    //readexactlytest(bv4,0);

    //readexactlytest(bv5,1);
    //readexactlytest(bv5,0);

    //    readuntiltest(bv4,"0123456",0);  // OK!
    //    readuntiltest(bv4,"x",0);  // OK!
    //    readuntiltest(bv4,"fz",0);  // OK!
    //    readuntiltest(bv4,"z0",0);  // OK!
    //    readuntiltest(bv4,"fz0",0);  // OK!
    //    readuntiltest(bv4,"efz",0);  // OK!
    //    readuntiltest(bv4,"z01",0);  //OK!
    //    readuntiltest(bv4,"9ab",0);  //OK!
    //readuntiltest(bv4,"def012",0);  //OK!
    //readuntiltest(bv4,"def012",0);  //OK
    readuntiltest(bv4,"f0123",0);  //OK!
    readuntiltest(bv4,"cdef0",0);  //OK!

    //readuntiltest(bv4,"9",1);
    //readuntiltest(bv4,"9a",0);

    //readuntiltest(bv5,,1);
    //readuntiltest(bv5,bv4,0);

    return 0;
}
 
