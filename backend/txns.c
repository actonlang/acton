/*
 * txns.c
 *
 *      Author: aagapi
 */

#ifndef BACKEND_TXNS_H_
#define BACKEND_TXNS_H_

#include "txns.h"

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

int rw_conflict(txn_read * tr, txn_write * tw)
{
	if(tr->table_key == tw->table_key)
	{
		// RW conflicts or regular data:
		if(tr->query_type >= QUERY_TYPE_READ_COLS && tr->query_type <= QUERY_TYPE_READ_INDEX_RANGE &&
			(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE) )
		{
			switch(tr->query_type)
			{
				case QUERY_TYPE_READ_COLS:
				{


					break;
				}
				case QUERY_TYPE_READ_CELL:
				{
					break;
				}
				case QUERY_TYPE_READ_CELL_RANGE:
				{
					break;
				}
				case QUERY_TYPE_READ_ROW:
				{
					break;
				}
				case QUERY_TYPE_READ_ROW_RANGE:
				{
					break;
				}
				case QUERY_TYPE_READ_INDEX:
				{
					break;
				}
				case QUERY_TYPE_READ_INDEX_RANGE:
				{
					break;
				}
			}
		}
	}

	// Note: There can be no RW conflicts on queues

	return 0;
}

int ww_conflict(txn_write * tw1, txn_write * tw2)
{

}

int is_read_invalidated(txn_read * tr, db_t * db)
{
	txn_write * dummy_tw_update = get_dummy_txn_write(QUERY_TYPE_UPDATE, tr->start_primary_keys, tr->no_primary_keys, tr->start_clustering_keys, tr->no_clustering_keys, tr->table_key, 0);

	for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
	{
		assert(node->value != NULL);

		txn_state * ts = (txn_state *) node->value;

		for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
		{
			assert(write_op_n->value != NULL);

			txn_write * tw = (txn_write *) write_op_n->value;

			if(rw_conflict(tr, tw) && ts->state == TXN_STATUS_VALIDATED)
			{
				return 1;
			}
		}
	}

	return 0;
}

int is_write_invalidated(txn_write * tw, db_t * db)
{
	for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
	{
		assert(node->value != NULL);

		txn_state * ts = (txn_state *) node->value;

		for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
		{
			assert(write_op_n->value != NULL);

			txn_write * tw2 = (txn_write *) write_op_n->value;

			if(ww_conflict(tw, tw2) && ts->state == TXN_STATUS_VALIDATED)
			{
				return 1;
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

			if(is_read_invalidated(tr, db))
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

			if(is_write_invalidated(tw, db))
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
