/*
 * skiplist.c
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>

#include "skiplist.h"
#include "fastrand.h"

int long_cmp(WORD e1, WORD e2) {
	return (int) (e1 - e2);
}

skiplist_t *create_skiplist_long() {
	return create_skiplist(NULL);
}

skiplist_t *create_skiplist(int (*cmp)(WORD, WORD)) {
	skiplist_t * list = (skiplist_t *)malloc(sizeof(skiplist_t));

	return skiplist_init(list, cmp);
}

skiplist_t *skiplist_init(skiplist_t *list, int (*cmp)(WORD, WORD)) {
    int i;
    snode_t *header = (snode_t *) malloc(sizeof(struct snode));
    list->header = header;
    header->key = (WORD) LONG_MAX;
    header->forward = (snode_t **) malloc(sizeof(snode_t*) * (SKIPLIST_MAX_LEVEL));
    for (i = 0; i < SKIPLIST_MAX_LEVEL; i++) {
        header->forward[i] = NULL; // list->header;
    }

    list->level = 0;

    list->no_items=0;

    list->cmp = (cmp != NULL)?(cmp):(&long_cmp);

    return list;
}

static int rand_level(unsigned int * seedptr) {
	unsigned int randno;

	FASTRAND(seedptr, randno);

	return randno % SKIPLIST_MAX_LEVEL;
}

int skiplist_insert(skiplist_t *list, WORD key, WORD value, unsigned int * seedptr) {
    snode_t *update[SKIPLIST_MAX_LEVEL];
    snode_t *x = list->header;
    int i, level;
    for (i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) < 0))
            x = x->forward[i];
//		printf("Item %ld will update node %ld at level %d\n", key, x->key, i);
        	update[i] = x;
    }
//    x = x->forward[0];

    if (x != NULL && list->cmp(key, x->key) == 0) {
        x->value = value;
        return 0;
    } else {
        level = rand_level(seedptr);
//		printf("Item %ld, picking level %d\n", key, level);
        if (level > list->level) {
            for (i = list->level + 1; i <= level; i++) {
                update[i] = list->header;
            }
            list->level = level;
        }

        x = (snode_t *) malloc(sizeof(snode_t));
        x->key = key;
        x->value = value;
        x->forward = (snode_t **) malloc(sizeof(snode_t*) * (level+1));
        for (i = 0; i <= level; i++) {
//        		printf("Item %ld chaining myself after node %ld at level %d\n", key, update[i]->key, i);
            x->forward[i] = update[i]->forward[i];
            update[i]->forward[i] = x;
        }

        list->no_items++;
    }
    return 0;
}

snode_t *skiplist_search(skiplist_t *list, WORD key) {
    snode_t *x = list->header;
    int i;
    for (i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) <= 0) )
            x = x->forward[i];
    }

    if (x != NULL && list->cmp(key, x->key) == 0) {
        return x;
    } else {
        return NULL;
    }

    return NULL;
}

snode_t *skiplist_search_higher(skiplist_t *list, WORD key) {
    snode_t *x = list->header;
    int i;
    for (i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) < 0))
            x = x->forward[i];
    }

    if(x != NULL)
    		return x->forward[0];
    else
    		assert(0);

    return NULL;
}

int skiplist_get_range(skiplist_t *list, WORD start_key, WORD end_key, WORD** result, int *no_nodes)
{
	snode_t * start_node = NULL;
	int i=0;

	start_node = skiplist_search_higher(list, start_key);

	if(start_node != NULL)
	{
		*result = NULL;
		*no_nodes = 0;
		return -1;
	}

	*no_nodes=1;

	for(snode_t * x = start_node;list->cmp(x->forward[0]->key, end_key) < 0;x = x->forward[0])
		(*no_nodes)++;

	*result = (WORD*) malloc(*no_nodes*sizeof(WORD));

	for(snode_t * x = start_node;list->cmp(x->forward[0]->key, end_key) < 0;x = x->forward[0])
		(*result)[i++] = x->value;

	return 0;
}


snode_t *skiplist_search_lower(skiplist_t *list, WORD key) {
    snode_t *x = list->header;
    int i;
    for (i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) <= 0))
            x = x->forward[i];
    }

    return x;
}


static void skiplist_node_free(snode_t *x) {
    if (x) {
        free(x->forward);
        free(x);
    }
}

WORD skiplist_delete(skiplist_t *list, WORD key) {
    int i;
    WORD value = NULL;

    snode_t *update[SKIPLIST_MAX_LEVEL];
    snode_t *x = list->header;
    for (i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) < 0))
            x = x->forward[i];
        	update[i] = x;
    }

    x = x->forward[0];
    if (list->cmp(x->key, key) == 0) {
        for (i = 0; i <= list->level; i++) {
            if (update[i]->forward[i] != x)
                break;
            update[i]->forward[i] = x->forward[i];
        }

        value = x->value;

        skiplist_node_free(x);

        while (list->level > 0 && list->header->forward[list->level] == NULL)
            list->level--;

        list->no_items--;
    }
    return value;
}

void skiplist_free(skiplist_t *list)
{
    snode_t *current_node = list->header->forward[0];
    while(current_node != NULL) {
        snode_t *next_node = current_node->forward[0];
        free(current_node->forward);
        free(current_node);
        current_node = next_node;
    }
//    free(current_node->forward);
//    free(current_node);
    free(list);
}

void skiplist_dump(skiplist_t *list) {
    snode_t *x = list->header;
    while (x && x->forward[0] != NULL) {
        printf("%ld[%ld]->", (long) x->forward[0]->key, (long) x->forward[0]->value);
        x = x->forward[0];
    }
    printf("NIL\n");
}



