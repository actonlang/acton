/*
 * server.c
 *
 *      Author: aagapi
 */

// Server:

db_schema_t * get_schema(db_t * db, WORD table_key)
{
	snode_t * node = skiplist_search(db->tables, (long) table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table->schema;
}

int get_ack_packet(int status, write_query * q,
					void ** snd_buf, unsigned * snd_msg_len)
{
	ack_message * ack = init_ack_message(get_cell_address(q->cell), status, q->txnid, q->nonce);
	return serialize_ack_message(ack, snd_buf, snd_msg_len);
}

int handle_write_query(write_query * wq, db_t * db, unsigned int * fastrandstate)
{
	int i=0;
	int total_columns = wq->cell->no_keys + wq->cell->no_columns;
	WORD * column_values = (WORD *) malloc(total_columns * sizeof(WORD));
	for(;i<wq->cell->no_keys;i++)
		column_values[i] = (WORD) wq->cell->keys[i];
	for(;i<total_columns;i++)
		column_values[i] = (WORD) wq->cell->keys[i-wq->cell->no_keys];

	return db_insert(column_values, total_columns, (WORD) wq->cell->table_key, db, fastrandstate);
}

int handle_write_packet(void * rcv_buf, unsigned rcv_msg_len,
						void ** snd_buf, unsigned * snd_msg_len,
						db_t * db, unsigned int * fastrandstate)
{
	write_query * q;
	int success = deserialize_write_query(rcv_buf, rcv_msg_len, &q);
	if(success != 0)
	{
		return get_ack_packet(success, q, snd_buf, snd_msg_len);
	}
	success = handle_write_query(q, db, fastrandstate);
	return get_ack_packet(success, q, snd_buf, snd_msg_len);
}

vector_clock * get_empty_vc()
{
	int node_ids[] = {0};
	long counters[] = {0};
	return init_vc(1, node_ids, counters, 0);
}

int get_read_response_packet(db_row_t* result, read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
	int no_keys = schema->no_primary_keys + schema->no_clustering_keys;
	int no_columns = schema->no_cols - no_keys;
	long * keys = (long *) malloc(no_keys);
	long * columns = (long *) malloc(no_columns);
	for(int i = 0;i < no_keys;i++)
		keys[i] = q->cell_address->keys[i];
	for(int i = 0;i < no_columns;i++)
		columns[i] = result->column_array[i];
	vector_clock * vc = get_empty_vc();

	cell * c = init_cell(q->cell_address->table_key, keys, no_keys, columns, no_columns, vc);
	read_response_message * m = init_write_query(c, q->txnid, q->nonce);
	return serialize_write_query(m, snd_buf, snd_msg_len);
}

db_row_t* handle_read_query(read_query * q, db_t * db, db_schema_t ** schema, unsigned int * fastrandstate)
{
	int i=0;

	*schema = get_schema(db, (WORD) q->cell_address->table_key);
	WORD * primary_keys = (WORD *) malloc((*schema)->no_primary_keys * sizeof(WORD));
	int no_clustering_keys = q->cell_address->no_keys - (*schema)->no_primary_keys;
	WORD * clustering_keys = (WORD *) malloc(no_clustering_keys * sizeof(WORD));

	for(;i<(*schema)->no_primary_keys;i++)
		primary_keys[i] = (WORD) q->cell_address->keys[i];
	for(;i<q->cell_address->no_keys;i++)
		clustering_keys[i-(*schema)->no_primary_keys] = (WORD) q->cell_address->keys[i];

	db_row_t* result = db_search_clustering(primary_keys, clustering_keys, no_clustering_keys, (WORD) q->cell_address->table_key, db);

	return result;
}

int handle_read_packet(void * buf, unsigned msg_len, db_t * db,
						void ** snd_buf, unsigned * snd_msg_len,
						unsigned int * fastrandstate)
{
	read_query * q;
	db_schema_t * schema;
	int success = deserialize_read_query(buf, msg_len, &q);
	db_row_t* result = handle_read_query(q, db, &schema, fastrandstate);
	return get_read_response_packet(result, q, schema, snd_buf, snd_msg_len);
}



