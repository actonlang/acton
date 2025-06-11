/*
 * Copyright (C) 2019-2021 Deutsche Telekom AG
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

/*
 * skiplist_test.c
 *
 */

#include "backend/skiplist.h"
#include <stdio.h>

int main() {
    int arr[] = { 1, 3, 3, 6, 9, 9, 2, 11, 11, 1, 4, 4 }, i;
    skiplist_t * list;
    unsigned int randno;

    list = create_skiplist_long();

    printf("Insert:--------------------\n");
    for (i = 0; i < sizeof(arr) / sizeof(arr[0]); i++) {
        skiplist_insert(list, (WORD) arr[i], (WORD) arr[i], &randno);
        skiplist_dump(list);
    }
//    skiplist_dump(list);

    printf("Search:--------------------\n");
    int keys[] = { 3, 4, 7, 10, 111 };

    for (i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
        snode_t *x = skiplist_search(list, (WORD) keys[i]);
        if (x) {
            printf("key = %d, value = %d\n", keys[i], (int) x->value);
        } else {
            printf("key = %d, not found\n", keys[i]);
        }
    }

    printf("Search:--------------------\n");
    skiplist_delete(list, (WORD) 3);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 9);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 11);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 11);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 11);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 1);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 6);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 2);
    skiplist_dump(list);
    skiplist_delete(list, (WORD) 4);
    skiplist_dump(list);
    skiplist_free(list);

    return 0;
}



