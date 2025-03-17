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
 * skiplist.c
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <uuid/uuid.h>
#include <inttypes.h>

#include "backend/skiplist.h"
#include "backend/log.h"
#include "backend/fastrand.h"

int long_cmp(WORD e1, WORD e2) {
    return (int) ((int64_t) e1 - (int64_t) e2);
}

int uuid_cmp(WORD e1, WORD e2)
{
    return uuid_compare(e1, e2);
}

skiplist_t *create_skiplist_long() {
    return create_skiplist(NULL);
}

skiplist_t *create_skiplist_uuid() {
    return create_skiplist(&uuid_cmp);
}

skiplist_t *create_skiplist(int (*cmp)(WORD, WORD)) {
    skiplist_t * list = (skiplist_t *) malloc(sizeof(skiplist_t));

    return skiplist_init(list, cmp);
}

skiplist_t *skiplist_init(skiplist_t *list, int (*cmp)(WORD, WORD)) {
    snode_t *header = (snode_t *) malloc(sizeof(struct snode));
    list->header = header;
    header->key = (WORD) LONG_MAX;
    header->forward = (snode_t **) malloc(sizeof(snode_t*) * (SKIPLIST_MAX_LEVEL));
    for (int i = 0; i < SKIPLIST_MAX_LEVEL; i++) {
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
    int i = list->level, level;
    for (; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) < 0))
            x = x->forward[i];
//      log_debug("Item %" PRId64 " will update node %" PRId64 " at level %d", key, x->key, i);
            update[i] = x;
    }

    if (x->forward[0] != NULL && list->cmp(key, x->forward[0]->key) == 0) {
        x->forward[0]->value = value;
        return 0;
    } else {
        level = rand_level(seedptr);
//      log_debug("Item %" PRId64 ", picking level %d", key, level);
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
//              log_debug("Item %" PRId64 " chaining myself after node %" PRId64 " at level %d", key, update[i]->key, i);
            x->forward[i] = update[i]->forward[i];
            update[i]->forward[i] = x;
        }

        list->no_items++;
    }
    return 0;
}

skiplist_t * skiplist_clone(skiplist_t * list, unsigned int * seedptr)
{
    skiplist_t * dst_list = create_skiplist(list->cmp);

    for(snode_t * crt = HEAD(list); crt!=NULL; crt = NEXT(crt))
    {
        skiplist_insert(dst_list, crt->key, crt->value, seedptr);
    }

    return dst_list;
}

snode_t *skiplist_search(skiplist_t *list, WORD key) {
    snode_t *x = list->header;
    for (int i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) <= 0) )
            x = x->forward[i];
    }

    if (x != NULL && ((int64_t) x->key) != LONG_MAX && list->cmp(key, x->key) == 0) {
        return x;
    } else {
        return NULL;
    }

    return NULL;
}

snode_t *skiplist_search_higher(skiplist_t *list, WORD key) {
    snode_t *x = list->header;
    for (int i = list->level; i >= 0; i--) {
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
    for (int i = list->level; i >= 0; i--) {
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
    WORD value = NULL;

    snode_t *update[SKIPLIST_MAX_LEVEL];
    snode_t *x = list->header;
    for (int i = list->level; i >= 0; i--) {
        while (x->forward[i] != NULL && (list->cmp(x->forward[i]->key, key) < 0))
            x = x->forward[i];
            update[i] = x;
    }

    if(x->forward[0] != NULL)
            x = x->forward[0];

    if (list->cmp(x->key, key) == 0) {
        for (int i = 0; i <= list->level; i++) {
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

    free(list->header->forward);
    free(list->header);

    free(list);
}

void skiplist_free_val(skiplist_t *list, void (*free_val)(WORD))
{
    snode_t *current_node = list->header->forward[0];
    while(current_node != NULL) {
        snode_t *next_node = current_node->forward[0];
        free(current_node->forward);
        free_val(current_node->value);
        free(current_node);
        current_node = next_node;
    }

    free(list->header->forward);
    free(list->header);

    free(list);
}

void skiplist_dump(skiplist_t *list) {
    snode_t *x = list->header;
    while (x && x->forward[0] != NULL) {
        log_trace("%" PRId64 "[%" PRId64 "]->", (int64_t) x->forward[0]->key, (int64_t) x->forward[0]->value);
        x = x->forward[0];
    }
    log_trace("NIL");
}



