/*
 * consumer_state.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_CONSUMER_STATE_H_
#define BACKEND_CONSUMER_STATE_H_

#include "common.h"
#include "failure_detector/vector_clock.h"

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
