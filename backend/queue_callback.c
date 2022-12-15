/*
 * queue_callback.c
 *      Author: aagapi
 */

#include "queue_callback.h"

queue_callback_args * get_queue_callback_args(WORD table_key, WORD queue_id, WORD app_id, WORD shard_id, WORD consumer_id, WORD group_id, int status)
{
    queue_callback_args * qca = (queue_callback_args *) malloc(sizeof(queue_callback_args));
    qca->table_key = table_key;
    qca->queue_id = queue_id;

    qca->app_id = app_id;
    qca->shard_id = shard_id;
    qca->consumer_id = consumer_id;

    qca->group_id = group_id;

    qca->status = status;

    return qca;
}

void free_queue_callback_args(queue_callback_args * qca)
{
    free(qca);
}

queue_callback * get_queue_callback(void (*callback)(queue_callback_args *))
{
    queue_callback * qc = (queue_callback *) malloc(sizeof(queue_callback) + sizeof(pthread_mutex_t) + sizeof(pthread_cond_t));
    qc->lock = (pthread_mutex_t *) ((char *)qc + sizeof(queue_callback));
    qc->signal = (pthread_cond_t *) ((char *)qc + sizeof(queue_callback) + sizeof(pthread_mutex_t));
    pthread_mutex_init(qc->lock, NULL);
    pthread_cond_init(qc->signal, NULL);
    qc->callback = callback;
    return qc;
}

int wait_on_queue_callback(queue_callback * qc)
{
    int ret = pthread_mutex_lock(qc->lock);

#if DEBUG_QUEUE_CALLBACK > 0
    printf("Locked consumer lock %p/%p\n", qc, qc->lock);
#endif

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += 3;
    ret = pthread_cond_timedwait(qc->signal, qc->lock, &ts);

    pthread_mutex_unlock(qc->lock);

#if DEBUG_QUEUE_CALLBACK > 0
    printf("Unlocked consumer lock %p/%p\n", qc, qc->lock);
#endif

    return ret;
}

void free_queue_callback(queue_callback * qc)
{
    free(qc);
}


