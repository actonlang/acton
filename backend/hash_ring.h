/*
 * hash_ring.h
 *  - Thread safe consistent hashing API
 *      Author: aagapi
 */

#ifndef BACKEND_HASH_RING_H_
#define BACKEND_HASH_RING_H_

#include "common.h"
#include "skiplist.h"

#define BUCKET_LIVE 0
#define BUCKET_DEAD 1

typedef struct hash_ring
{
    skiplist_t * buckets;
    int live_buckets;
    pthread_mutex_t * lock;
} hash_ring;

hash_ring * get_hash_ring();
void free_hash_ring(hash_ring * ring, void (*free_val)(WORD));
int add_bucket(hash_ring * ring, WORD bucket, void * (*get_key)(void *), void * (*get_live_field)(void *), unsigned int * fastrandstate);
snode_t * lookup_bucket(hash_ring * ring, WORD bucket_id);
int get_bucket_status(hash_ring * ring, WORD bucket, void * (*get_key)(void *), void * (*get_live_field)(void *));
int set_bucket_status(hash_ring * ring, WORD bucket, int status, void * (*get_key)(void *), void * (*get_live_field)(void *));
int mark_bucket_dead(hash_ring * ring, WORD bucket, void * (*get_key)(void *), void * (*get_live_field)(void *));
int mark_bucket_live(hash_ring * ring, WORD bucket, void * (*get_key)(void *), void * (*get_live_field)(void *));
WORD get_buckets_for_object(hash_ring * ring, int object_id, int replication_factor,
                            void * (*get_key)(void *), void * (*get_live_field)(void *),
                            unsigned int * fastrandstate);

#endif /* BACKEND_HASH_RING_H_ */
