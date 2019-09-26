/*
 * skiplist.h
 *
 */

#ifndef BACKEND_SKIPLIST_H_
#define BACKEND_SKIPLIST_H_

typedef void *WORD;

#define SKIPLIST_MAX_LEVEL 6

#define HEAD(skiplist) ((skiplist)->header)
#define NEXT(snode) ((snode)->forward[0])

typedef struct snode {
    long key;
    WORD value;
    struct snode **forward;
} snode_t;

typedef struct skiplist {
    int level;
    struct snode *header;
    int no_items;
} skiplist_t;

skiplist_t * create_skiplist();
skiplist_t * skiplist_init(skiplist_t *list);
int skiplist_insert(skiplist_t *list, long key, WORD value, unsigned int * seedptr);
snode_t * skiplist_search(skiplist_t *list, long key);
snode_t *skiplist_search_higher(skiplist_t *list, long key);
snode_t *skiplist_search_lower(skiplist_t *list, long key);
int skiplist_get_range(skiplist_t *list, long start_key, long end_key, WORD** result, int *no_nodes);
static void skiplist_node_free(snode_t *x);
WORD skiplist_delete(skiplist_t *list, long key);
void skiplist_free(skiplist_t *list);
void skiplist_dump(skiplist_t *list);


#endif /* BACKEND_SKIPLIST_H_ */
