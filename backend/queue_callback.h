/*
 * queue_callback.h
 *      Author: aagapi
 */

#ifndef BACKEND_QUEUE_CALLBACK_H_
#define BACKEND_QUEUE_CALLBACK_H_

#include "common.h"

typedef struct queue_callback_args
{
	WORD table_key;
	WORD queue_id;

	WORD consumer_id;
	WORD shard_id;
	WORD app_id;

	WORD group_id;

	int status;
} queue_callback_args;

typedef struct queue_callback
{
	void (*callback)(queue_callback_args *);
	pthread_mutex_t * lock;
	pthread_cond_t * signal;
} queue_callback;

#define DEBUG_QUEUE_CALLBACK 0

queue_callback_args * get_queue_callback_args(WORD table_key, WORD queue_id, WORD app_id, WORD shard_id, WORD consumer_id, WORD group_id, int status);
void free_queue_callback_args(queue_callback_args * qca);
queue_callback * get_queue_callback(void (*callback)(queue_callback_args *));
int wait_on_queue_callback(queue_callback *);
void free_queue_callback(queue_callback * qc);

#endif /* BACKEND_QUEUE_CALLBACK_H_ */
