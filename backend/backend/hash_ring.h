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
 * hash_ring.h
 *  - Thread safe consistent hashing API
 *      Author: aagapi
 */

#ifndef BACKEND_HASH_RING_H_
#define BACKEND_HASH_RING_H_

#include "backend/common.h"
#include "backend/skiplist.h"

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
