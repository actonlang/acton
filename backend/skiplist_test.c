/*
 * skiplist_test.c
 *
 */

#include "skiplist.h"

int main() {
    int arr[] = { 3, 6, 9, 2, 11, 1, 4 }, i;
    skiplist_t * list;
    list = (skiplist_t *)malloc(sizeof(skiplist_t));
    skiplist_init(list);

    printf("Insert:--------------------\n");
    for (i = 0; i < sizeof(arr) / sizeof(arr[0]); i++) {
        skiplist_insert(list, arr[i], arr[i]);
    }
    skiplist_dump(list);

    printf("Search:--------------------\n");
    int keys[] = { 3, 4, 7, 10, 111 };

    for (i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
        snode_t *x = skiplist_search(list, keys[i]);
        if (x) {
            printf("key = %d, value = %d\n", keys[i], x->value);
        } else {
            printf("key = %d, not fuound\n", keys[i]);
        }
    }

    printf("Search:--------------------\n");
    skiplist_delete(list, 3);
    skiplist_delete(list, 9);
    skiplist_dump(list);
    skiplist_free(list);

    return 0;
}



