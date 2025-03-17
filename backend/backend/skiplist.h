/*
 * skiplist.h
 *
 */

#ifndef BACKEND_SKIPLIST_H_
#define BACKEND_SKIPLIST_H_

typedef void *WORD;

#define SKIPLIST_MAX_LEVEL 6

#define HEAD(skiplist) ((skiplist)->header->forward[0])
#define NEXT(snode) ((snode)->forward[0])

typedef struct snode {
    WORD key;
    WORD value;
    struct snode **forward;
} snode_t;

typedef struct skiplist {
    int level;
    struct snode *header;
    int no_items;

    int (*cmp)(WORD, WORD);
} skiplist_t;

skiplist_t * create_skiplist_long();
skiplist_t *create_skiplist_uuid();
skiplist_t * create_skiplist(int (*cmp)(WORD, WORD));
skiplist_t * skiplist_init(skiplist_t *list, int (*cmp)(WORD, WORD));
skiplist_t * skiplist_clone(skiplist_t * list, unsigned int * seedptr);
int skiplist_insert(skiplist_t *list, WORD key, WORD value, unsigned int * seedptr);
snode_t * skiplist_search(skiplist_t *list, WORD key);
snode_t *skiplist_search_higher(skiplist_t *list, WORD key);
snode_t *skiplist_search_lower(skiplist_t *list, WORD key);
int skiplist_get_range(skiplist_t *list, WORD start_key, WORD end_key, WORD** result, int *no_nodes);
static void skiplist_node_free(snode_t *x);
WORD skiplist_delete(skiplist_t *list, WORD key);
void skiplist_free(skiplist_t *list);
void skiplist_free_val(skiplist_t *list, void (*free_val)(WORD));
void skiplist_dump(skiplist_t *list);


#endif /* BACKEND_SKIPLIST_H_ */
