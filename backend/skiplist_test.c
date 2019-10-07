/*
 * skiplist_test.c
 *
 */

#include "skiplist.h"
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
    skiplist_free(list);

    return 0;
}



