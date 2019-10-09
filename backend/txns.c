/*
 * txns.c
 *
 *      Author: aagapi
 */

#ifndef BACKEND_TXNS_H_
#define BACKEND_TXNS_H_

#include "txns.h"

int verbose = 1;

// DB queries:

txn_state * get_txn_state(uuid_t * txnid, db_t * db)
{
	return (txn_state *) skiplist_search(db->txn_state, (WORD) txnid);
}

uuid_t * new_txn(db_t * db, unsigned int * seedptr)
{
	txn_state * ts = NULL, * previous = NULL;

	while(ts == NULL)
	{
		ts = init_txn_state();
		previous = get_txn_state(&(ts->txnid), db);
		if(previous != NULL)
		{
			free_txn_state(ts);
			ts = NULL;
		}
	}

	skiplist_insert(db->txn_state, (WORD) &(ts->txnid), (WORD) ts, seedptr);

	return &(ts->txnid);
}

int close_txn(uuid_t * txnid, db_t * db)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	skiplist_delete(db->txn_state, txnid);
	free_txn_state(ts);

	return 0;
}

int key_path_overlaps(txn_read * tr, txn_write * tw)
{
	// Writes must be at least as specific as reads:
	assert(tr->no_primary_keys + tr->no_clustering_keys <= tw->no_primary_keys + tw->no_clustering_keys);

	int is_exact_read_query = (tr->query_type == QUERY_TYPE_READ_COLS || tr->query_type == QUERY_TYPE_READ_CELL || tr->query_type == QUERY_TYPE_READ_ROW);

	if(tr->query_type == QUERY_TYPE_READ_INDEX)
	{
		if((long) tw->column_values[tr->idx_idx] == (long) tr->start_primary_keys[0])
			return 1;
		return 0;
	}

	if(tr->query_type == QUERY_TYPE_READ_INDEX_RANGE)
	{
		if((long) tw->column_values[tr->idx_idx] <= (long) tr->start_primary_keys[0] && (long) tw->column_values[tr->idx_idx] >= (long) tr->end_primary_keys[0])
			return 1;
		return 0;
	}

	for(int i=0;i<tr->no_primary_keys;i++)
	{
		if(is_exact_read_query && tr->start_primary_keys[i] != tw->column_values[i])
			return 0;
		if(!is_exact_read_query &&
			((long) tr->start_primary_keys[i] > (long) tw->column_values[i] ||
			(long) tr->end_primary_keys[i] < (long) tw->column_values[i]))
				return 0;
	}
	for(int i=0;i<tr->no_clustering_keys;i++)
	{
		if(is_exact_read_query && tr->start_clustering_keys[i] != tw->column_values[tw->no_primary_keys + i])
			return 0;
		if(!is_exact_read_query &&
			((long) tr->start_clustering_keys[i] > (long) tw->column_values[tw->no_primary_keys + i] ||
			(long) tr->end_clustering_keys[i] < (long) tw->column_values[tw->no_primary_keys + i]))
				return 0;
	}

	return 1;
}

// Check if txn read op tr is invalidated by write op tw:
int rw_conflict(txn_read * tr, txn_write * tw, int check_exact_match)
{
	if(check_exact_match && tr->table_key != tw->table_key)
		return 0;

	// Check for RW conflicts only on regular data (all conflicts on queues are WW):

	if(tr->query_type >= QUERY_TYPE_READ_COLS && tr->query_type <= QUERY_TYPE_READ_INDEX_RANGE &&
		(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE) )
	{
		switch(tr->query_type)
		{
			case QUERY_TYPE_READ_COLS:
			case QUERY_TYPE_READ_CELL:
			case QUERY_TYPE_READ_CELL_RANGE:
			case QUERY_TYPE_READ_ROW:
			case QUERY_TYPE_READ_ROW_RANGE:
			case QUERY_TYPE_READ_INDEX:
			case QUERY_TYPE_READ_INDEX_RANGE:
			{
				if(check_exact_match && key_path_overlaps(tr, tw)) //  && compare_vc(tr->version, vector_clock * vc2)
					return 1;
			}
		}
	}

	return 0;
}

// Check if queue op tw1 is invalidated by queue op tw2:
int queue_op_conflict(txn_write * tw1, txn_write * tw2)
{
	// Subscribes and unsubscribes are not currently supported in txns

	assert(tw1->query_type != QUERY_TYPE_SUBSCRIBE_QUEUE && tw1->query_type != QUERY_TYPE_UNSUBSCRIBE_QUEUE);
	assert(tw2->query_type != QUERY_TYPE_SUBSCRIBE_QUEUE && tw2->query_type != QUERY_TYPE_UNSUBSCRIBE_QUEUE);

	// Queue ops only conflict if they are on the same table and queue:

	if(tw1->table_key != tw2->table_key || tw1->queue_id != tw2->queue_id)
		return 0;

	// CREATE_QUEUE and DELETE_QUEUE conflict with everything:

	if(tw1->query_type == QUERY_TYPE_CREATE_QUEUE || tw1->query_type == QUERY_TYPE_DELETE_QUEUE ||
	   tw2->query_type == QUERY_TYPE_CREATE_QUEUE || tw2->query_type == QUERY_TYPE_DELETE_QUEUE)
		return 1;

	// Only ENQUEUE/ENQUEUE (by any producers), as well as READ_QUEUE / READ_QUEUE and
	// CONSUME_QUEUE / CONSUME_QUEUE *by the same consumer* are valid conflicts:

	if(tw1->query_type != tw2->query_type)
		return 0;

	if(tw1->query_type == QUERY_TYPE_ENQUEUE)
		return 1;

	if((tw1->query_type == QUERY_TYPE_READ_QUEUE || tw1->query_type == QUERY_TYPE_CONSUME_QUEUE) &&
			tw1->consumer_id == tw2->consumer_id &&
			tw1->shard_id == tw2->shard_id &&
			tw1->app_id == tw2->app_id)
		return 1;

	return 0;
}

// Check if txn write op tw1 is invalidated by write op tw2:
int ww_conflict(txn_write * tw1, txn_write * tw2)
{
	short is_regular_op1 = (tw1->query_type == QUERY_TYPE_UPDATE || tw1->query_type == QUERY_TYPE_DELETE);
	short is_regular_op2 = (tw2->query_type == QUERY_TYPE_UPDATE || tw2->query_type == QUERY_TYPE_DELETE);

	if((is_regular_op1 && !is_regular_op2) || (!is_regular_op1&&is_regular_op2))
		return 0;

	if(is_regular_op1)
		return (txn_write_cmp((WORD) tw1, (WORD) tw2) == 0);
	else
		return queue_op_conflict(tw1, tw2);
}

int is_read_invalidated(txn_read * tr, txn_state * rts, db_t * db)
{
	// If exact (non-range) read, and not a secondary index query, create dummy write op on the same key path, to look it up in other txn's write set:

	int is_exact_query = (tr->query_type == QUERY_TYPE_READ_COLS || tr->query_type == QUERY_TYPE_READ_CELL || tr->query_type == QUERY_TYPE_READ_ROW);
	txn_write * dummy_tw_update = is_exact_query? get_dummy_txn_write(QUERY_TYPE_UPDATE, tr->start_primary_keys, tr->no_primary_keys, tr->start_clustering_keys, tr->no_clustering_keys, tr->table_key, 0) : NULL;

	for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
	{
		assert(node->value != NULL);

		txn_state * ts = (txn_state *) node->value;

		if(ts->state != TXN_STATUS_VALIDATED || uuid_compare(rts->txnid, ts->txnid) == 0)
			continue;

		if(is_exact_query)
		{
			snode_t * write_op_n = skiplist_search(ts->write_set, (WORD) dummy_tw_update);

			if(write_op_n != NULL)
			{
				assert(write_op_n->value != NULL);

				txn_write * tw = (txn_write *) write_op_n->value;

				if(rw_conflict(tr, tw, 0))
				{
					if(verbose)
						printf("Invalidating txn due to rw conflict\n");
					return 1;
				}
			}
		}
		else
		// For range or index queries, we need to iterate the other txn write sets to see if any writes conflict with our reads:
		{
			for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
			{
				assert(write_op_n->value != NULL);

				txn_write * tw = (txn_write *) write_op_n->value;

				if(rw_conflict(tr, tw, 1) && ts->state == TXN_STATUS_VALIDATED)
				{
					return 1;
				}
			}
		}
	}

	return 0;
}

int is_write_invalidated(txn_write * tw, txn_state * rts, db_t * db)
{
	for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
	{
		assert(node->value != NULL);

		txn_state * ts = (txn_state *) node->value;

		if(ts->state != TXN_STATUS_VALIDATED || uuid_compare(rts->txnid, ts->txnid) == 0)
			continue;

		if(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE)
		// Regular ops
		{
			snode_t * write_op_n = skiplist_search(ts->write_set, (WORD) tw);

			if(write_op_n != NULL)
			{
				assert(write_op_n->value != NULL);

				txn_write * tw2 = (txn_write *) write_op_n->value;

//				if(ww_conflict(tw, tw2, 0))
//				{
					if(verbose)
						printf("Invalidating txn due to ww conflict\n");
					return 1;
//				}
			}
		}
		else
		// Queue ops:
		{
			for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
			{
				assert(write_op_n->value != NULL);

				txn_write * tw2 = (txn_write *) write_op_n->value;

				if(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE)
					continue;

				if(queue_op_conflict(tw, tw2))
				{
					return 1;
				}
			}
		}
	}

	return 0;
}


int validate_txn(uuid_t * txnid, db_t * db)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	assert(ts->state == TXN_STATUS_ACTIVE);

	for(snode_t * read_op_n=HEAD(ts->read_set); read_op_n!=NULL; read_op_n=NEXT(read_op_n))
	{
		if(read_op_n->value != NULL)
		{
			txn_read * tr = (txn_read *) read_op_n->value;

			if(is_read_invalidated(tr, ts, db))
			{
				return VAL_STATUS_ABORT;
			}
		}
	}

	for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
	{
		if(write_op_n->value != NULL)
		{
			txn_write * tw = (txn_write *) write_op_n->value;

			if(is_write_invalidated(tw, ts, db))
			{
				return VAL_STATUS_ABORT;
			}
		}
	}

	ts->state = TXN_STATUS_VALIDATED;

	return VAL_STATUS_COMMIT;
}

int persist_write(txn_write * tw, db_t * db, unsigned int * fastrandstate)
{
	switch(tw->query_type)
	{
		case QUERY_TYPE_UPDATE:
		{
			return db_insert(tw->column_values, tw->no_cols, tw->table_key, db, fastrandstate);
		}
		case QUERY_TYPE_DELETE:
		{
			return db_delete_row(tw->column_values, tw->table_key, db); // TO DO: use tw->no_primary_keys and tw->no_clustering_keys
		}
		case QUERY_TYPE_ENQUEUE:
		{
			return enqueue(tw->column_values, tw->no_cols, tw->table_key, tw->queue_id, 1, db, fastrandstate);
		}
		case QUERY_TYPE_READ_QUEUE:
		{
			return set_private_read_head(tw->consumer_id, tw->shard_id, tw->app_id, tw->table_key, tw->queue_id, tw->new_read_head, 1, db);
		}
		case QUERY_TYPE_CONSUME_QUEUE:
		{
			return set_private_consume_head(tw->consumer_id, tw->shard_id, tw->app_id, tw->table_key, tw->queue_id, tw->new_consume_head, db);
		}
		case QUERY_TYPE_CREATE_QUEUE:
		{
			return create_queue(tw->table_key, tw->queue_id, 1, db, fastrandstate);
		}
		case QUERY_TYPE_DELETE_QUEUE:
		{
			return delete_queue(tw->table_key, tw->queue_id, 1, db);
		}
		default:
		{
			assert(0);
		}
	}

	return 0;
}

int persist_txn(txn_state * ts, db_t * db, unsigned int * fastrandstate)
{
	int res = 0;

	for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
	{
		if(write_op_n->value != NULL)
		{
			txn_write * tw = (txn_write *) write_op_n->value;
			res = persist_write(tw, db, fastrandstate);
			assert (res == 0);
		}
	}

	return 0;
}

int abort_txn(uuid_t * txnid, db_t * db)
{
	return close_txn(txnid, db);
}

int commit_txn(uuid_t * txnid, db_t * db)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	int res = validate_txn(txnid, db);

	if(res == VAL_STATUS_COMMIT)
	{
		persist_txn(ts, db);
//		ts->state = TXN_STATUS_COMMITTED;
		close_txn(txnid, db);
	}
	else if(res == VAL_STATUS_ABORT)
	{
		abort_txn(txnid, db);
	}
	else
	{
		assert(0);
	}

	return 0;
}


int db_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_write_to_txn(QUERY_TYPE_UPDATE, column_values, no_cols, no_primary_keys, no_clustering_keys, table_key, ts, fastrandstate);
}

db_row_t* db_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	db_row_t* result = db_search(primary_keys, table_key, db);

	// Note that if result == NULL (no such row), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

	return add_row_read_to_txn(primary_keys, no_primary_keys, table_key, result, ts, fastrandstate);
}

int db_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
							snode_t** start_row, snode_t** end_row,
							WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	int ret = db_range_search(start_primary_keys, end_primary_keys, start_row, end_row, table_key, db);

	// Note that if ret == 0 (no rows read), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

	return add_row_range_read_to_txn(start_primary_keys, end_primary_keys, no_primary_keys, table_key, *start_row, *end_row, ts, fastrandstate);
}


db_row_t* db_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	db_row_t* result = db_search_clustering(primary_keys, clustering_keys, no_clustering_keys, table_key, db);

	// Note that if result == NULL (no such row), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

	return add_cell_read_to_txn(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys, table_key, result, ts, fastrandstate);
}

int db_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	// Note that if ret == 0 (no rows read), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

	db_range_search_clustering(primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, start_row, end_row, table_key, db);

	return add_cell_range_read_to_txn(primary_keys, no_primary_keys, start_clustering_keys,
										end_clustering_keys, no_clustering_keys, table_key,
										*start_row, *end_row, ts, fastrandstate);
}

// WORD* db_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int* column_idxs, int no_columns, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
db_row_t* db_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD* col_keys, int no_columns, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	assert (0); // This won't work until db_search_columns is refactored as per below:
	return 0;

/*
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

//	db_search_columns(primary_keys, clustering_keys, int* column_idxs, no_columns, table_key, db_t * db);

	db_row_t* result = db_search_columns_result(primary_keys, clustering_keys, col_keys, no_columns, table_key, db);

	return add_col_read_to_txn(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys,
								col_keys, no_columns, table_key, result, ts, fastrandstate);
*/
}

db_row_t* db_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	db_row_t* result = db_search_index(index_key, idx_idx, table_key, db);

	return add_index_read_to_txn(&index_key, idx_idx, table_key, result, ts, fastrandstate);
}

int db_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	db_range_search_index(idx_idx, start_idx_key, end_idx_key, start_row, end_row, table_key, db);

	return add_index_range_read_to_txn(idx_idx, &start_idx_key, &end_idx_key, *start_row, *end_row, table_key, ts, fastrandstate);
}

int db_delete_row_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_write_to_txn(QUERY_TYPE_DELETE, primary_keys, no_primary_keys, no_primary_keys, 0, table_key, ts, fastrandstate);
}

int db_delete_cell_in_txn(WORD* keys, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_write_to_txn(QUERY_TYPE_DELETE, keys, no_primary_keys+no_clustering_keys, no_primary_keys, no_clustering_keys, table_key, ts, fastrandstate);
}

int db_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	assert (0); // Not supported yet
	return 0;

/*
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn
*/
}

int db_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	assert (0); // Not supported in new schema-less model
	return 0;
}

// Queue ops:

int enqueue_in_txn(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_enqueue_to_txn(column_values, no_cols, table_key, queue_id, ts, fastrandstate);
}

int read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, uuid_t * txnid,
		db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	long prev_read_head = -1;

	// Lookup previously existing read head in this txn's read set:

	for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
	{
		if(write_op_n->value != NULL)
		{
			txn_write * tw = (txn_write *) write_op_n->value;

			if(tw->query_type == QUERY_TYPE_READ_QUEUE && tw->table_key == table_key &&
				tw->queue_id == queue_id && tw->consumer_id == consumer_id &&
				tw->shard_id == shard_id && tw->app_id == app_id)
			{
				prev_read_head = tw->new_read_head;
				break;
			}
		}
	}

	peek_queue(consumer_id, shard_id, app_id, table_key, queue_id,
			max_entries, prev_read_head, entries_read, new_read_head,
			start_row, end_row, db);

	return add_read_queue_to_txn(consumer_id, shard_id, app_id, table_key, queue_id,
									*new_read_head, ts, fastrandstate);
}

int consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_consume_queue_to_txn(consumer_id, shard_id, app_id, table_key, queue_id,
			new_consume_head, ts, fastrandstate);
}

int subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	assert (0); // Not implemented
	return 0;
}

int unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
								uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	assert (0); // Not implemented
	return 0;
}

int create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_create_queue_to_txn(table_key, queue_id, ts, fastrandstate);
}

int delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
	txn_state * ts = get_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	return add_delete_queue_to_txn(table_key, queue_id, ts, fastrandstate);
}

#endif /* BACKEND_TXNS_H_ */
