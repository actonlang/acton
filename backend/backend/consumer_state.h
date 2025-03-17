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
 * consumer_state.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_CONSUMER_STATE_H_
#define BACKEND_CONSUMER_STATE_H_

#include "backend/common.h"
#include "backend/failure_detector/vector_clock.h"

typedef struct group_state group_state;

typedef struct group_queue_consumer_state {
    int64_t private_read_head;
    int64_t private_consume_head;

    vector_clock * prh_version;
    vector_clock * pch_version;

    group_state * gs;
} group_queue_consumer_state;

typedef struct consumer_state {
    WORD consumer_id;
    WORD shard_id;
    WORD app_id;
    WORD group_id;

    int64_t private_read_head;
    int64_t private_consume_head;

    vector_clock * prh_version;
    vector_clock * pch_version;

    short notified;

    queue_callback* callback; // For local subscribers
    int * sockfd; // For remote subscribers
} consumer_state;

#endif /* BACKEND_CONSUMER_STATE_H_ */
