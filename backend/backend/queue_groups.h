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
 * queue_groups.h
 *      Author: aagapi
 */

#ifndef BACKEND_QUEUE_GROUPS_H_
#define BACKEND_QUEUE_GROUPS_H_

#include "backend/common.h"
#include "backend/queue_callback.h"
#include "backend/queue.h"
#include "backend/skiplist.h"

#define GROUP_STATUS_ACTIVE 0
#define GROUP_STATUS_INACTIVE 1

typedef struct group_state {
    WORD group_id;
    skiplist_t * queue_tables;
    skiplist_t * consumers;
    pthread_mutex_t* group_lock;
    int status;
} group_state;

typedef struct consumer_state consumer_state;

// Queue group management fctns:

group_state * get_group(WORD group_id);
int delete_group(group_state * group);
int clear_group(group_state * group);
void activate_group(group_state * group);
void deactivate_group(group_state * group);
int add_queue_to_group(group_state * group, WORD table_key, WORD queue_id, unsigned int * fastrandstate);
int remove_queue_from_group(group_state * group, WORD table_key, WORD queue_id);
int add_listener_to_group(group_state * group,
                        WORD consumer_id, WORD shard_id, WORD app_id,
                        queue_callback * callback,
                        int * sockfd,
                        unsigned int * fastrandstate);
int lookup_listener_in_group(group_state * group, WORD consumer_id, WORD queue_id, consumer_state ** cs);
int remove_listener_from_group(group_state * group, WORD consumer_id);
int is_queue_in_group(group_state * group, WORD table_key, WORD queue_id);
void free_group_state(WORD gs);
WORD get_group_state_key(WORD rs);
WORD get_group_state_live_field(WORD rs);

#endif /* BACKEND_QUEUE_GROUPS_H_ */
