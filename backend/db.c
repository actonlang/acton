/*
 * db.c
 *
 *      Author: aagapi
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>

#include "db.h"
#include "skiplist.h"

// DB API:

db_row_t * create_empty_row(WORD key)
{
	db_cell_t * row = (db_cell_t *) malloc(sizeof(db_cell_t));

	row->key = key;

	row->cells = NULL;

	row->column_array = NULL;

	row->no_columns=0;

	row->_next = NULL;

	return row;
}

db_row_t * create_db_row(WORD * column_values, db_schema_t * schema, unsigned int * fastrandstate)
{
	assert(schema->no_primary_keys == 1);

	db_cell_t * row = create_empty_row(column_values[schema->primary_key_idxs[0]]);

	// Several clustering keys mean several levels of depth (a la super columns):

	db_cell_t * crt_cell = row, * new_cell = NULL;

	for(int i=0; i<schema->no_clustering_keys; i++, crt_cell = new_cell)
	{
		crt_cell->cells = create_skiplist_long();

		new_cell = create_empty_row(column_values[schema->clustering_key_idxs[i]]);

		if(i == schema->no_clustering_keys - 1)
		{
			new_cell->no_columns = schema->no_cols - schema->no_primary_keys - schema->no_clustering_keys;
			new_cell->column_array = (WORD *) malloc(new_cell->no_columns);
			for(int j=0;j<new_cell->no_columns;j++)
			{
				new_cell->column_array[j] = column_values[schema->no_primary_keys + schema->no_clustering_keys + j];
			}
		}

		skiplist_insert(crt_cell->cells, column_values[schema->clustering_key_idxs[i]], (WORD) new_cell, fastrandstate);
	}

	return row;
}

void free_db_cell(db_row_t * row, int depth)
{
	if(depth > 0)
	{
		assert(row->cells != NULL);

		for(snode_t * cell=HEAD(row->cells);cell!=NULL;cell=NEXT(cell))
			if(cell->value != NULL)
				free_db_cell(cell->value, depth-1);

		skiplist_free(row->cells);
	}

	if(row->column_array != NULL)
		free(row->column_array);

	free(row);
}

void free_db_row(db_row_t * row, db_schema_t * schema)
{
	assert(schema->no_primary_keys == 1);

	free_db_cell(row, schema->no_clustering_keys);
}

db_t * get_db()
{
	db_t * db = (db_t *) malloc(sizeof(db_t));

	db->tables = create_skiplist_long();
	db->txn_state = create_skiplist_uuid();

	return db;
}

int db_delete_db(db_t * db)
{
	skiplist_free(db->tables);
	skiplist_free(db->txn_state);

	free(db);

	return 0;
}

int db_dump_db(db_t * db)
{
	assert(0 && "DB dump not implemented yet");

	return 0;
}

// Deep copy constructor (to allow caller to free his structs):
db_schema_t* db_create_schema(int * col_types, int no_cols, int * primary_key_idxs, int no_primary_keys, int * clustering_key_idxs, int no_clustering_keys, int * index_key_idxs, int no_index_keys)
{
	assert(no_cols > 0 && "Schema must have at least 1 column");
	assert(no_primary_keys > 0 && "Schema must have at least 1 primary key");
	assert(no_primary_keys <= no_cols && "Schema must have less primary keys than columns");
	assert(no_primary_keys == 1 && "Schemas don't currently support compound primary keys");
	assert(primary_key_idxs[0] < no_cols && "Primary key index out of bounds");

	db_schema_t * schema = (db_schema_t *) malloc(sizeof(db_schema_t));

	schema->col_types = (int *) malloc(no_cols * sizeof(int));
	schema->no_cols = no_cols;
	for(int i=0;i<no_cols;i++)
		schema->col_types[i] = col_types[i];

	schema->primary_key_idxs = (int *) malloc(no_primary_keys * sizeof(int));
	schema->no_primary_keys = no_primary_keys;
	for(int i=0;i<no_primary_keys;i++)
		schema->primary_key_idxs[i] = primary_key_idxs[i];

	schema->no_clustering_keys = no_clustering_keys;
	if(no_clustering_keys > 0)
	{
		schema->clustering_key_idxs = (int *) malloc(no_clustering_keys * sizeof(int));
		for(int i=0;i<no_clustering_keys;i++)
			schema->clustering_key_idxs[i] = clustering_key_idxs[i];
	}

	schema->no_index_keys = no_index_keys;
	if(no_index_keys > 0)
	{
		schema->index_key_idxs = (int *) malloc(no_index_keys * sizeof(int));
		for(int i=0;i<no_index_keys;i++)
			schema->index_key_idxs[i] = index_key_idxs[i];
	}

	return schema;
}

void free_schema(db_schema_t * schema)
{
	free(schema->col_types);
	free(schema->primary_key_idxs);
	if(schema->no_clustering_keys > 0)
		free(schema->clustering_key_idxs);
	if(schema->no_index_keys > 0)
		free(schema->index_key_idxs);
	free(schema);
}

int db_create_table(WORD table_key, db_schema_t* schema, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = (db_table_t *) malloc(sizeof(db_table_t));

	// Deep copy of schema (to allow caller to free his copy):
	table->schema = db_create_schema(schema->col_types, schema->no_cols, schema->primary_key_idxs, schema->no_primary_keys, schema->clustering_key_idxs, schema->no_clustering_keys, schema->index_key_idxs, schema->no_index_keys);

	table->rows = create_skiplist_long();

	if(schema->no_index_keys > 0)
		table->indexes = (skiplist_t **) malloc(schema->no_index_keys * sizeof(skiplist_t *));

	for(int i=0;i<schema->no_index_keys;i++)
		table->indexes[i] = create_skiplist_long();

	table->lock = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(table->lock, NULL);

	return skiplist_insert(db->tables, table_key, (WORD) table, fastrandstate);
}

int db_create_index(int new_index, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
	assert(0 && "Index creation post schema creation not supported yet");

	return 0;
}

int db_delete_table(WORD table_key, db_t * db)
{
	db_table_t * table = (db_table_t *) skiplist_delete(db->tables, table_key);

	if(table != NULL)
	{
		skiplist_free(table->rows);
		for(int i=0;i<table->schema->no_index_keys;i++)
			skiplist_free(table->indexes[i]);
		if(table->schema->no_index_keys > 0)
			free(table->indexes);
		free_schema(table->schema);
		free(table);
	}

	return table != NULL;
}


// Table API:

int table_insert(WORD * column_values, int no_cols, db_table_t * table, unsigned int * fastrandstate)
{
	db_schema_t * schema = table->schema;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	assert(no_cols == schema->no_cols && "Row insert must contain all schema columns");

	db_row_t * row = NULL;
	snode_t * row_node = skiplist_search(table->rows, column_values[schema->primary_key_idxs[0]]);

	if(row_node == NULL)
	{
		row = create_db_row(column_values, schema, fastrandstate);
		skiplist_insert(table->rows, column_values[schema->primary_key_idxs[0]], (WORD) row, fastrandstate);
	}
	else
	{
		row = (db_row_t *) row_node->value;

		db_row_t * cell = row, * new_cell = NULL;

		for(int i=0;i<schema->no_clustering_keys;i++, cell = new_cell)
		{
			snode_t * new_cell_node = skiplist_search(cell->cells, column_values[schema->clustering_key_idxs[i]]);

			if(new_cell_node == NULL)
			{
				new_cell = create_empty_row(column_values[schema->clustering_key_idxs[i]]);

				if(i == schema->no_clustering_keys - 1)
				{
					new_cell->no_columns = schema->no_cols - schema->no_primary_keys - schema->no_clustering_keys;
					new_cell->column_array = (WORD *) malloc(new_cell->no_columns);
					for(int j=0;j<new_cell->no_columns;j++)
					{
						new_cell->column_array[j] = column_values[schema->no_primary_keys + schema->no_clustering_keys + j];
					}
				}
				else
				{
					new_cell->cells = create_skiplist_long();
				}

//				printf("Inserting into cell at level %d\n", i);

				skiplist_insert(cell->cells, column_values[schema->clustering_key_idxs[i]], (WORD) new_cell, fastrandstate);
			}
			else
			{
				new_cell = (db_row_t *) (new_cell_node->value);
			}
		}
	}

	for(int i=0;i<schema->no_index_keys;i++)
		skiplist_insert(table->indexes[i], column_values[schema->index_key_idxs[i]], (WORD) row, fastrandstate);

	return 0;
}

int table_insert_sf(WORD * column_values, int no_cols, db_table_t * table, unsigned int * fastrandstate)
{
	db_schema_t * schema = table->schema;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	assert(no_cols == schema->no_cols && "Row insert must contain all schema columns");

	db_row_t * row = NULL;
	snode_t * row_node = skiplist_search(table->rows, column_values[schema->primary_key_idxs[0]]);

	if(row_node == NULL)
	{
		row = create_db_row(column_values, schema, fastrandstate);
		skiplist_insert(table->rows, column_values[schema->primary_key_idxs[0]], (WORD) row, fastrandstate);
	}
	else
	{
		row = (db_row_t *) row_node->value;

		db_row_t * cell = row, * new_cell = NULL;

		for(int i=0;i<schema->no_clustering_keys;i++, cell = new_cell)
		{
			snode_t * new_cell_node = skiplist_search(cell->cells, column_values[schema->clustering_key_idxs[i]]);

			if(new_cell_node == NULL)
			{
				new_cell = create_empty_row(column_values[schema->clustering_key_idxs[i]]);

				if(i == schema->no_clustering_keys - 1)
				{
					new_cell->no_columns = schema->no_cols - schema->no_primary_keys - schema->no_clustering_keys;
					new_cell->column_array = (WORD *) malloc(new_cell->no_columns);
					for(int j=0;j<new_cell->no_columns;j++)
					{
						new_cell->column_array[j] = column_values[schema->no_primary_keys + schema->no_clustering_keys + j];
					}
				}
				else
				{
					new_cell->cells = create_skiplist_long();
				}

//				printf("Inserting into cell at level %d\n", i);

				skiplist_insert(cell->cells, column_values[schema->clustering_key_idxs[i]], (WORD) new_cell, fastrandstate);
			}
			else
			{
				new_cell = (db_row_t *) (new_cell_node->value);
			}
		}
	}

	for(int i=0;i<schema->no_index_keys;i++)
		skiplist_insert(table->indexes[i], column_values[schema->index_key_idxs[i]], (WORD) row, fastrandstate);

	return 0;
}


int table_update(int * col_idxs, int no_cols, WORD * column_values, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	assert(no_cols > schema->no_primary_keys + schema->no_clustering_keys && "Empty update");

	assert(col_idxs[0] == schema->primary_key_idxs[0] && "Update must contain primary key as first element");

	for(int i=0;i<schema->no_clustering_keys;i++)
	{
		assert(col_idxs[i+1] == schema->clustering_key_idxs[i] && "Update must contain all clustering keys in the right order, right after primary key");
	}

	db_row_t * row = NULL;
	snode_t * row_node = skiplist_search(table->rows, column_values[schema->primary_key_idxs[0]]);

	if(row_node == NULL)
		return -1;

	row = (db_row_t *) row_node->value;

	for(int i=0;i<schema->no_clustering_keys;i++)
	{
		row_node = skiplist_search(row->cells, column_values[schema->clustering_key_idxs[i]]);

		if(row_node == NULL)
			return -1;

		row = (db_row_t *) (row_node->value);
	}

	for(int i=schema->no_primary_keys + schema->no_clustering_keys;i<no_cols;i++)
	{
//		printf("Updating col %d / %d to value %ld\n", col_idxs[i], i, column_values[i]);
		row->column_array[col_idxs[i] - schema->no_primary_keys - schema->no_clustering_keys] = column_values[i];
	}

	return 0;
}

db_row_t* table_search(WORD* primary_keys, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	snode_t * row_node = skiplist_search(table->rows, primary_keys[0]);

	if(row_node == NULL)
		return NULL;

	db_row_t* row = (db_row_t *) row_node->value;

	return row;
}

int table_range_search(WORD* start_primary_keys, WORD* end_primary_keys, snode_t** start_row, snode_t** end_row, db_table_t * table)
{
	db_schema_t * schema = table->schema;
	int no_results = 0;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	*start_row = skiplist_search_higher(table->rows, start_primary_keys[0]);

	for(*end_row = *start_row; (long) (*end_row)->key < (long) end_primary_keys[0]; *end_row=NEXT(*end_row), no_results++);

	return no_results+1;
}

int table_range_search_copy(WORD* start_primary_keys, WORD* end_primary_keys, db_row_t** rows, db_table_t * table)
{
	db_schema_t * schema = table->schema;
	int no_results = 0;

	assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

	return skiplist_get_range(table->rows, start_primary_keys[0], end_primary_keys[0], (WORD**) rows, &no_results);
}

db_row_t* table_search_clustering(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(no_clustering_keys > 0 && "No clustering keys given");

	assert(no_clustering_keys <= schema->no_clustering_keys && "Too many clustering keys given");

	db_row_t* row = table_search(primary_keys, table);

//	if(row == NULL)
//		printf("Row not found by primary key %ld!\n", (long) primary_keys[0]);

	for(int i=0;i<no_clustering_keys;i++)
	{
		snode_t * row_node = skiplist_search(row->cells, clustering_keys[i]);

		if(row_node != NULL)
		{
			row = (db_row_t *) row_node->value;
		}
		else
		{
//			printf("Row not found by clustering key %d / %ld!\n", i, (long) clustering_keys[i]);

			return NULL;
		}
	}

	return row;
}

int table_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, db_table_t * table)
{
	db_schema_t * schema = table->schema;
	int no_results = 0;

	assert(no_clustering_keys > 0 && "No clustering keys given");

	assert(no_clustering_keys <= schema->no_clustering_keys && "Too many clustering keys given");

	db_row_t* row = table_search(primary_keys, table);

	for(int i=0;i<no_clustering_keys-1;i++)
	{
		assert(start_clustering_keys[i] == end_clustering_keys[i] && "For first N-1 clustering keys, start key must be equal to end key");

		snode_t * row_node = skiplist_search(row->cells, start_clustering_keys[i]);

		if(row_node != NULL)
		{
			row = (db_row_t *) row_node->value;
		}
		else
		{
			return -1;
		}
	}

	*start_row = skiplist_search_higher(row->cells, start_clustering_keys[no_clustering_keys-1]);

	for(*end_row = *start_row; (*end_row) != NULL && (long) (*end_row)->key < (long) end_clustering_keys[no_clustering_keys-1]; *end_row=NEXT(*end_row), no_results++);

	return no_results+1;
}

WORD* table_search_columns(WORD* primary_keys, WORD* clustering_keys, int* column_idxs, int no_columns, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(no_columns > 0 && "No column indexes given");

	db_row_t* row = table_search_clustering(primary_keys, clustering_keys, schema->no_clustering_keys, table);

	if(row == NULL)
		return NULL;

	WORD* results = (WORD*) malloc(no_columns * sizeof(WORD));

	for(int i=0;i<no_columns;i++)
	{
		assert(column_idxs[i] <= schema->no_cols && "Column index doesn't exist in schema");
		assert(column_idxs[i] <= row->no_columns + schema->no_primary_keys + schema->no_clustering_keys && "Column index doesn't exist in backend (DB corrupted?)");

		if(column_idxs[i] < schema->no_primary_keys)
			results[i] = primary_keys[column_idxs[i]];
		else if(column_idxs[i] < schema->no_primary_keys + schema->no_clustering_keys)
			results[i] = clustering_keys[column_idxs[i] - schema->no_primary_keys];
		else
			results[i] = row->column_array[column_idxs[i] - schema->no_primary_keys - schema->no_clustering_keys];
	}

	return results;
}

db_row_t* table_search_index(WORD index_key, int idx_idx, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

	snode_t * row_node = skiplist_search(table->indexes[idx_idx], index_key);

	if(row_node != NULL)
	{
		return (db_row_t *) (row_node->value);
	}
	else
	{
		return NULL;
	}
}

int table_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, db_table_t * table)
{
	db_schema_t * schema = table->schema;
	int no_results = 0;

	assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

	*start_row = skiplist_search_higher(table->indexes[idx_idx], start_idx_key);

	for(*end_row = *start_row; (*end_row != NULL) && ((long) (*end_row)->key < (long) end_idx_key); *end_row=NEXT(*end_row), no_results++);

	return no_results+1;
}

int table_delete_row(WORD* primary_keys, db_table_t * table)
{
	db_row_t* row = (db_row_t *) (skiplist_delete(table->rows, primary_keys[0]));

	if(row != NULL)
	{
		free_db_row(row, table->schema);
	}
	else
	{
		printf("table_delete_row(): Row with pk %ld doesn't exist!\n", (long) primary_keys[0]);
	}

	return row == NULL;
}

int table_delete_by_index(WORD index_key, int idx_idx, db_table_t * table)
{
	db_schema_t * schema = table->schema;

	assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

	db_row_t* row = (db_row_t *) (skiplist_delete(table->indexes[idx_idx], index_key));

	// TO DO: Re-enable this after enhancing indexes:

//	if(row != NULL)
//		free_db_row(row, table->schema);

	return row == NULL;
}


// DB API:

int db_insert(WORD * column_values, int no_cols, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_insert(column_values, no_cols, table, fastrandstate);
}

int db_update(int * col_idxs, int no_cols, WORD * column_values, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_update(col_idxs, no_cols, column_values, table);
}

db_row_t* db_search(WORD* primary_keys, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table_search(primary_keys, table);
}

int db_range_search(WORD* start_primary_keys, WORD* end_primary_keys, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_range_search(start_primary_keys, end_primary_keys, start_row, end_row, table);
}

int db_range_search_copy(WORD* start_primary_keys, WORD* end_primary_keys, db_row_t** rows, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_range_search_copy(start_primary_keys, end_primary_keys, rows, table);
}

db_row_t* db_search_clustering(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table_search_clustering(primary_keys, clustering_keys, no_clustering_keys, table);
}

int db_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_range_search_clustering(primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, start_row, end_row, table);
}

WORD* db_search_columns(WORD* primary_keys, WORD* clustering_keys, int* column_idxs, int no_columns, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table_search_columns(primary_keys, clustering_keys, column_idxs, no_columns, table);
}

db_row_t* db_search_index(WORD index_key, int idx_idx, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table_search_index(index_key, idx_idx, table);
}

int db_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_range_search_index(idx_idx, start_idx_key, end_idx_key, start_row, end_row, table);
}

int db_delete_row(WORD* primary_keys, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
	{
		printf("db_delete_row(): Table with pk %ld doesn't exist!\n", (long) table_key);
		return -1;
	}

	db_table_t * table = (db_table_t *) (node->value);

	return table_delete_row(primary_keys, table);
}

int db_delete_by_index(WORD index_key, int idx_idx, WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return -1;

	db_table_t * table = (db_table_t *) (node->value);

	return table_delete_by_index(index_key, idx_idx, table);
}




