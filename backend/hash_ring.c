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
 * hash_ring.c
 *  - Thread safe consistent hashing API
 *      Author: aagapi
 */

#include "backend/hash_ring.h"
#include "backend/hashes.h"

hash_ring * get_hash_ring()
{
    hash_ring * ring = (hash_ring *) malloc(sizeof(hash_ring) + sizeof(pthread_mutex_t));
    memset(ring, 0, sizeof(hash_ring) + sizeof(pthread_mutex_t));
    ring->buckets = create_skiplist_long();
    ring->live_buckets = 0;
    ring->lock = (pthread_mutex_t*) ((char*) ring + sizeof(hash_ring));
    pthread_mutex_init(ring->lock, NULL);
    return ring;
}

void free_hash_ring(hash_ring * ring, void (*free_val)(WORD))
{
    if(free_val == NULL)
    {
        skiplist_free(ring->buckets);
    }
    else
    {
        skiplist_free_val(ring->buckets, free_val);
    }
    free(ring);
}

int add_bucket(hash_ring * ring, WORD bucket, void* (*get_key)(void *), void* (*get_live_field)(void *), unsigned int * fastrandstate)
{
    pthread_mutex_lock(ring->lock);

    uint32_t id_hash = hash32((uint32_t) get_key(bucket));

    if(skiplist_search(ring->buckets, (WORD) id_hash) != NULL)
    {
        pthread_mutex_unlock(ring->lock);
        return -1;
    }

    if(*((int *) get_live_field(bucket)) == BUCKET_LIVE)
        ring->live_buckets++;

    int status = skiplist_insert(ring->buckets, (WORD) id_hash, bucket, fastrandstate);

    pthread_mutex_unlock(ring->lock);

    return status;
}

snode_t * lookup_bucket(hash_ring * ring, WORD bucket_id)
{
    WORD id_hash = (WORD) hash32((uint32_t) bucket_id);
    return skiplist_search(ring->buckets, id_hash);
}

int get_bucket_status(hash_ring * ring, WORD bucket, void* (*get_key)(void *), void* (*get_live_field)(void *))
{
    snode_t * node = NULL;
    pthread_mutex_lock(ring->lock);

    uint32_t id_hash = hash32((uint32_t) get_key(bucket));

    node = skiplist_search(ring->buckets, (WORD) id_hash);

    if(node == NULL)
    {
        pthread_mutex_unlock(ring->lock);
        return -1;
    }

    int status = *((int *) get_live_field(node->value));

    pthread_mutex_unlock(ring->lock);

    return status;
}

int set_bucket_status(hash_ring * ring, WORD bucket, int status, void* (*get_key)(void *), void* (*get_live_field)(void *))
{
    snode_t * node = NULL;
    pthread_mutex_lock(ring->lock);

    uint32_t id_hash = hash32((uint32_t) bucket); // hash32((uint32_t) get_key(bucket));

    node = skiplist_search(ring->buckets, (WORD) id_hash);

    if(node == NULL)
    {
        pthread_mutex_unlock(ring->lock);
        return -1;
    }

    if(*((int *) get_live_field(node->value)) != status)
    {
        if(status == BUCKET_LIVE)
            ring->live_buckets++;
        else
            ring->live_buckets--;
    }

    *((int *) get_live_field(node->value)) = status;

    pthread_mutex_unlock(ring->lock);

    return 0;
}

int mark_bucket_dead(hash_ring * ring, WORD bucket, void* (*get_key)(void *), void* (*get_live_field)(void *))
{
    return set_bucket_status(ring, bucket, BUCKET_DEAD, get_key, get_live_field);
}

int mark_bucket_live(hash_ring * ring, WORD bucket, void* (*get_key)(void *), void* (*get_live_field)(void *))
{
    return set_bucket_status(ring, bucket, BUCKET_LIVE, get_key, get_live_field);
}

WORD get_buckets_for_object(hash_ring * ring, int object_id, int replication_factor,
                            void* (*get_key)(void *), void* (*get_live_field)(void *),
                            unsigned int * fastrandstate)
{
    pthread_mutex_lock(ring->lock);

    skiplist_t * result = (replication_factor > 1)?create_skiplist_long():NULL;
    int status = 0;

    int replicas = (replication_factor < ring->live_buckets)?replication_factor:ring->live_buckets;

    if(replicas == 0)
    {
        pthread_mutex_unlock(ring->lock);

        return NULL;
    }

    snode_t * snode = skiplist_search_higher(ring->buckets, (WORD) hash32((uint32_t) object_id));

    while(result == NULL || result->no_items < replicas)
    {
        if(snode == NULL)
        {
            // Wrap back to the beginning of the ring:
            snode = HEAD(ring->buckets);
        }

        if(*((int *) get_live_field(snode->value)) == BUCKET_LIVE)
        {
            if(replication_factor > 1)
            {
                status = skiplist_insert(result, (WORD) get_key(snode->value), snode->value, fastrandstate);
                assert(status == 0);
            }

            pthread_mutex_unlock(ring->lock);

            return (WORD) snode->value;
        }

        snode = NEXT(snode);
    }

    pthread_mutex_unlock(ring->lock);

    return (WORD) result;
}

