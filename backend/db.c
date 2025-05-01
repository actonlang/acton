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
 * db.c
 *
 *      Author: aagapi
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <assert.h>

#include "backend/db.h"
#include "backend/queue_groups.h"
#include "backend/skiplist.h"
#include "backend/log.h"

// DB API:

db_row_t * create_empty_row(WORD key)
{
    db_cell_t * row = (db_cell_t *) malloc(sizeof(db_cell_t));

    memset(row, 0, sizeof(db_cell_t));

    row->key = key;

    row->cells = NULL;

    row->column_array = NULL;

    row->no_columns=0;

    row->last_blob_size = 0;

    row->version = NULL;

    row->_next = NULL;

    row->no_entries = 0;

    return row;
}


db_row_t * create_db_row_schemaless(WORD * column_values, int * primary_key_idxs, int no_primary_keys,
                                    int * clustering_key_idxs, int no_clustering_keys, int no_schema_clustering_keys,
                                    int no_cols, size_t last_blob_size, unsigned int * fastrandstate)
{
    assert(no_primary_keys == 1);
    assert(no_clustering_keys >= no_schema_clustering_keys);

    db_cell_t * row = create_empty_row(column_values[primary_key_idxs[0]]);

    // Several clustering keys mean several levels of depth (a la super columns):

    db_cell_t * crt_cell = row, * new_cell = NULL;

    for(int i=0; i<no_clustering_keys; i++, crt_cell = new_cell)
    {
        crt_cell->cells = create_skiplist_long();

        int col_index = (i<no_schema_clustering_keys)?(clustering_key_idxs[i]):(i);

        new_cell = create_empty_row(column_values[col_index]);

        if(i == no_clustering_keys - 1)
        {
            new_cell->no_columns = no_cols - no_primary_keys - no_clustering_keys;
            new_cell->last_blob_size = last_blob_size;
            new_cell->column_array = (WORD *) malloc(new_cell->no_columns * sizeof(WORD));
            int j=0;
            for(;j<new_cell->no_columns - 1;j++)
            {
                new_cell->column_array[j] = column_values[no_primary_keys + no_clustering_keys + j];
            }

            if(last_blob_size <= 0) // last column is value
            {
                new_cell->column_array[j] = column_values[no_primary_keys + no_clustering_keys + j];
            }
            else // last column is blob
            {
                new_cell->column_array[j] = malloc(last_blob_size);
                memcpy(new_cell->column_array[j], column_values[no_primary_keys + no_clustering_keys + j], last_blob_size);
            }
        }

        skiplist_insert(crt_cell->cells, column_values[col_index], (WORD) new_cell, fastrandstate);
    }

    return row;
}

// Assumes key indexes are in order (partition keys, followed by clustering keys, followed by columns). Also assumes a single partition key:
db_row_t * create_db_row_schemaless2(WORD * keys, int no_keys, WORD * cols, int no_cols, WORD last_blob, size_t last_blob_size, unsigned int * fastrandstate)
{
    db_cell_t * row = create_empty_row(keys[0]);

    db_cell_t * crt_cell = row, * new_cell = NULL;
    for(int i=1; i<no_keys; i++, crt_cell = new_cell)
    {
        crt_cell->cells = create_skiplist_long();

        new_cell = create_empty_row(keys[i]);

        skiplist_insert(crt_cell->cells, keys[i], (WORD) new_cell, fastrandstate);
    }

    assert(crt_cell != NULL && crt_cell->cells == NULL);

    assert(last_blob == NULL || last_blob_size > 0);

    int total_cols = no_cols + ((last_blob != NULL)?1:0);

    crt_cell->no_columns = total_cols;
    crt_cell->column_array = (WORD *) malloc(total_cols * sizeof(WORD));
    int j=0;
    for(;j<no_cols;j++)
    {
        crt_cell->column_array[j] = cols[j];
    }

    crt_cell->last_blob_size = last_blob_size;

    if(last_blob != NULL)
    {
        assert(total_cols == no_cols + 1);

        crt_cell->column_array[no_cols] = malloc(last_blob_size);
        memcpy(crt_cell->column_array[no_cols], last_blob, last_blob_size);
    }

    return row;
}

db_row_t * create_db_row_sf(WORD * column_values, db_schema_t * schema, int no_clustering_keys, int no_cols, size_t last_blob_size, unsigned int * fastrandstate)
{
    return create_db_row_schemaless(column_values, schema->primary_key_idxs, schema->no_primary_keys,
                                        schema->clustering_key_idxs, no_clustering_keys, schema->min_no_clustering_keys,
                                        no_cols, last_blob_size, fastrandstate);
}

void free_db_cell(db_row_t * row, db_t * db)
{
    if(row->cells != NULL)
    {
        for(snode_t * cell=HEAD(row->cells);cell!=NULL;cell=NEXT(cell))
            if(cell->value != NULL)
                free_db_cell(cell->value, db);

        skiplist_free(row->cells);
    }

    if(row->column_array != NULL)
    {
        if(row->last_blob_size > 0 && row->no_columns > 0 && row->column_array[row->no_columns - 1] != NULL)
        {
            free(row->column_array[row->no_columns - 1]);
        }

        free(row->column_array);
    }

    if(row->group_subscriptions != NULL && db->queue_group_replication_factor > 1)
    {
        skiplist_free(row->group_subscriptions);
    }

    free(row);
}

void free_db_row(db_row_t * row, db_schema_t * schema, db_t * db)
{
    free_db_cell(row, db);
}

db_t * get_db()
{
#if (MULTI_THREADED == 1)
    db_t * db = (db_t *) malloc(sizeof(db_t) + sizeof(pthread_mutex_t));
#else
    db_t * db = (db_t *) malloc(sizeof(db_t));
# endif

    db->tables = create_skiplist_long();
    db->txn_state = create_skiplist_uuid();
    db->queue_groups = get_hash_ring();
    db->queue_group_replication_factor = 1;
    db->enable_auto_queue_group_subscriptions = ENABLE_AUTO_QUEUE_GROUP_SUBSCRIPTIONS;

#if (MULTI_THREADED == 1)
    db->txn_state_lock = (pthread_mutex_t*) ((char*) db + sizeof(db_t));
    pthread_mutex_init(db->txn_state_lock, NULL);
# endif

    return db;
}

int db_delete_db(db_t * db)
{
    skiplist_free(db->tables);
    skiplist_free(db->txn_state);
    free_hash_ring(db->queue_groups, free_group_state);
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

    schema->min_no_cols = no_cols;
    schema->col_types = NULL;

    if(col_types != NULL)
    {
        schema->col_types = (int *) malloc(no_cols * sizeof(int));
        for(int i=0;i<no_cols;i++)
            schema->col_types[i] = col_types[i];
    }

    schema->primary_key_idxs = (int *) malloc(no_primary_keys * sizeof(int));
    schema->no_primary_keys = no_primary_keys;
    for(int i=0;i<no_primary_keys;i++)
        schema->primary_key_idxs[i] = primary_key_idxs[i];

    schema->min_no_clustering_keys = no_clustering_keys;
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
    if(schema == NULL)
        return;

    if(schema->col_types != NULL)
        free(schema->col_types);
    if(schema->primary_key_idxs != NULL)
        free(schema->primary_key_idxs);
    if(schema->clustering_key_idxs != NULL)
        free(schema->clustering_key_idxs);
    if(schema->index_key_idxs != NULL)
        free(schema->index_key_idxs);
    free(schema);
}

int db_create_table(WORD table_key, db_schema_t* schema, db_t * db, unsigned int * fastrandstate)
{
    db_table_t * table = (db_table_t *) malloc(sizeof(db_table_t));

    table->table_key = table_key;

    // Deep copy of schema (to allow caller to free his copy):

    table->schema = db_create_schema(schema->col_types, schema->min_no_cols, schema->primary_key_idxs, schema->no_primary_keys, schema->clustering_key_idxs, schema->min_no_clustering_keys, schema->index_key_idxs, schema->no_index_keys);

    table->rows = create_skiplist_long();

    table->row_tombstones = create_skiplist_long();

    if(schema->no_index_keys > 0)
        table->indexes = (skiplist_t **) malloc(schema->no_index_keys * sizeof(skiplist_t *));

    for(int i=0;i<schema->no_index_keys;i++)
        table->indexes[i] = create_skiplist_long();

    table->queues = create_skiplist_long();

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
        skiplist_free(table->queues);
        free(table);
    }

    return table != NULL;
}


// Table API:

int table_insert(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, vector_clock * version, db_table_t * table, unsigned int * fastrandstate)
{
    db_schema_t * schema = table->schema;

    assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

    if(no_clustering_keys < schema->min_no_clustering_keys)
    {
        log_error("SERVER: Row insert must contain at least %d schema clustering keys, only has %d keys", schema->min_no_clustering_keys, no_clustering_keys);
        assert(0);
    }

    assert(no_cols > (schema->no_primary_keys + no_clustering_keys) && "Insert must contain at least 1 non-key column or blob");

    db_row_t * row = NULL;
    snode_t * row_node = skiplist_search(table->rows, column_values[schema->primary_key_idxs[0]]);

    if(row_node == NULL)
    {
        row = create_db_row_sf(column_values, schema, no_clustering_keys, no_cols, last_blob_size, fastrandstate);
        row->version = (version != NULL)? copy_vc(version) : NULL;
        skiplist_insert(table->rows, column_values[schema->primary_key_idxs[0]], (WORD) row, fastrandstate);
    }
    else
    {
        row = (db_row_t *) row_node->value;

        db_row_t * cell = row, * new_cell = NULL;

        for(int i=0;i<no_clustering_keys;i++, cell = new_cell)
        {
            int col_index = (i < schema->min_no_clustering_keys)?(schema->clustering_key_idxs[i]):(i);

            snode_t * new_cell_node = skiplist_search(cell->cells, column_values[col_index]);

            if(new_cell_node == NULL)
            {
                new_cell = create_empty_row(column_values[col_index]);

                if(i < no_clustering_keys - 1)
                {
                    new_cell->cells = create_skiplist_long();
                }

                skiplist_insert(cell->cells, column_values[col_index], (WORD) new_cell, fastrandstate);
            }
            else
            {
                new_cell = (db_row_t *) (new_cell_node->value);
            }
        }

        // Populate columns and set version for newly created cell:

        assert(cell != NULL && cell->cells == NULL);

        cell->no_columns = no_cols - schema->no_primary_keys - no_clustering_keys;
        cell->last_blob_size = last_blob_size;
        cell->column_array = (WORD *) malloc(cell->no_columns * sizeof(WORD));
        int j=0;
        for(;j<cell->no_columns - 1;j++)
        {
            cell->column_array[j] = column_values[schema->no_primary_keys + no_clustering_keys + j];
        }

        if(last_blob_size <= 0) // last column is value
        {
            cell->column_array[j] = column_values[schema->no_primary_keys + no_clustering_keys + j];
        }
        else // last column is blob
        {
            cell->column_array[j] = malloc(last_blob_size);;

            memcpy(cell->column_array[j], column_values[schema->no_primary_keys + no_clustering_keys + j], last_blob_size);
        }

        if(version != NULL)
            update_or_replace_vc(&(cell->version), version);
    }

    for(int i=0;i<schema->no_index_keys;i++)
    {
        if(schema->index_key_idxs[i] < no_cols)
            skiplist_insert(table->indexes[i], column_values[schema->index_key_idxs[i]], (WORD) row, fastrandstate);
    }

    return 0;
}


int table_update(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, vector_clock * version, db_table_t * table)
{
    db_schema_t * schema = table->schema;

    assert(schema->no_primary_keys == 1 && "Compound primary keys unsupported for now");

    assert(no_clustering_keys >= schema->min_no_clustering_keys);

    assert(no_cols > schema->no_primary_keys + no_clustering_keys && "Empty update");

    assert(col_idxs[0] == schema->primary_key_idxs[0] && "Update must contain primary key as first element");

    for(int i=0;i<schema->min_no_clustering_keys;i++)
    {
        assert(col_idxs[i+1] == schema->clustering_key_idxs[i] && "Update must contain all minimal clustering keys in the right order, right after primary key");
    }

    db_row_t * row = NULL;
    snode_t * row_node = skiplist_search(table->rows, column_values[schema->primary_key_idxs[0]]);

    if(row_node == NULL)
        return -1;

    row = (db_row_t *) row_node->value;

    for(int i=0;i<no_clustering_keys;i++)
    {
        row_node = skiplist_search(row->cells, column_values[schema->clustering_key_idxs[i]]);

        if(row_node == NULL)
            return -1;

        row = (db_row_t *) (row_node->value);
    }

    int i=schema->no_primary_keys + no_clustering_keys;
    for(;i<no_cols - 1;i++)
    {
        row->column_array[col_idxs[i] - schema->no_primary_keys - no_clustering_keys] = column_values[i];
    }

    if(last_blob_size <= 0) // last column is value
    {
        row->column_array[col_idxs[i] - schema->no_primary_keys - no_clustering_keys] = column_values[i];
    }
    else // last column is blob
    {
        row->column_array[col_idxs[i] - schema->no_primary_keys - no_clustering_keys] = malloc(last_blob_size);

        memcpy(row->column_array[col_idxs[i] - schema->no_primary_keys - no_clustering_keys], column_values[i], last_blob_size);
    }

    if(version != NULL)
        update_or_replace_vc(&(row->version), version);

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

    if(start_primary_keys == NULL)
        assert(end_primary_keys == NULL);

    if(start_primary_keys == NULL || (start_primary_keys[0] == (WORD)LONG_MIN && end_primary_keys[0] == (WORD)(LONG_MAX - 1)))
    {
//      assert(end_primary_keys == NULL);
        *start_row = HEAD(table->rows);
        if(table->rows->no_items > 0 && (*start_row) != NULL)
            for(*end_row=*start_row; NEXT(*end_row) != NULL; *end_row = NEXT(*end_row));

        return table->rows->no_items;
    }

    *start_row = skiplist_search_higher(table->rows, start_primary_keys[0]);

    if(*start_row == NULL)
    {
        *end_row = NULL;
        return 0;
    }

    for(*end_row = *start_row; NEXT(*end_row) != NULL && (int64_t) (*end_row)->key < (int64_t) end_primary_keys[0]; *end_row=NEXT(*end_row), no_results++);

    return no_results+1;
}

int table_verify_row_range_version(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                                        int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table)
{
    int i = 0;

    assert(no_primary_keys == 1 && "Compound primary keys unsupported for now");

    snode_t * start_row = skiplist_search_higher(table->rows, start_primary_keys[0]);

    for(snode_t * cell_row_node = start_row; cell_row_node != NULL && (int64_t) cell_row_node->key < (int64_t) end_primary_keys[0]; cell_row_node=NEXT(cell_row_node), i++)
    {
        db_row_t* cell_row = (db_row_t *) cell_row_node->value;

        // Some keys were removed from the backend since the range query happened:
        if(i>(no_range_results - 1))
            return 1;

        if((int64_t) cell_row->key != range_result_keys[i])
            return 1;

        int cmp = compare_vc(cell_row->version, range_result_versions[i]);
        if(cmp != 0)
            return cmp;
    }

    // Some extra keys were added to the backend since the range query happened:
    if(i<no_range_results)
        return 1;

    return 0;
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

//  assert(no_clustering_keys <= schema->min_no_clustering_keys && "Too many clustering keys given");

    db_row_t* row = table_search(primary_keys, table);

    if(row == NULL)
#if (VERBOSE_BACKEND > 0)
        log_error("Row not found by primary key %" PRId64 "!", (int64_t) primary_keys[0]);
#endif
        return NULL;

    for(int i=0;i<no_clustering_keys;i++)
    {
        snode_t * row_node = skiplist_search(row->cells, clustering_keys[i]);

        if(row_node != NULL)
        {
            row = (db_row_t *) row_node->value;
        }
        else
        {
#if (VERBOSE_BACKEND > 0)
            log_error("Row not found by clustering key %d / %" PRId64 "!", i, (int64_t) clustering_keys[i]);
#endif

            return NULL;
        }
    }

    return row;
}

int table_verify_cell_version(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, vector_clock * version, db_table_t * table)
{
    assert(no_primary_keys == 1);

    snode_t * row_node = skiplist_search(table->rows, primary_keys[0]);

    if(row_node == NULL)
        return DB_ERR_NO_QUEUE;

    db_row_t* row = (db_row_t *) row_node->value;

    for(int i=0;i<no_clustering_keys;i++)
    {
        snode_t * row_node = skiplist_search(row->cells, clustering_keys[i]);

        if(row_node == NULL)
            return DB_ERR_NO_QUEUE;

        row = (db_row_t *) row_node->value;
    }

    return compare_vc(version, row->version);
}

int table_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, db_table_t * table)
{
    db_schema_t * schema = table->schema;

    assert(no_clustering_keys > 0 && "No clustering keys given");

//  assert(no_clustering_keys <= schema->min_no_clustering_keys && "Too many clustering keys given");

    db_row_t* row = table_search(primary_keys, table);

    if(row == NULL)
        return 0;

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

    if(*start_row == NULL)
    {
        *end_row = NULL;
        return 0;
    }

    int no_results = 0;
    for(*end_row = *start_row; NEXT(*end_row) != NULL && (int64_t) (*end_row)->key < (int64_t) end_clustering_keys[no_clustering_keys-1]; *end_row=NEXT(*end_row), no_results++);

    return no_results+1;
}

void print_long_db(db_t * db)
{
    log_info("DB: [%d tables]", db->tables->no_items);

    for(snode_t * node = HEAD(db->tables);node!=NULL;node=NEXT(node))
        print_long_table((db_table_t *) node->value);
}

void print_long_table(db_table_t * table)
{
    log_info("DB_TABLE: %" PRId64 " [%d rows]", (int64_t) table->table_key, table->rows->no_items);

    for(snode_t * node = HEAD(table->rows);node!=NULL;node=NEXT(node))
        print_long_row((db_row_t*) node->value);
}

void print_long_row(db_row_t* row)
{
    char to_string[MAX_PRINT_BUFF];
    int len = 0;

    long_row_to_string(row, (char *) to_string, &len, (char *) to_string);

    log_info("DB_ROW [%d cells]: %s", (row->cells != NULL)?(row->cells->no_items):(0), to_string);
}

void long_row_to_string(db_row_t* row, char * to_string, int * len, char * orig_offset)
{
    #define PRINT_BLOBS 1
    #define PRINT_BLOBS_AS_LONG 1

    sprintf(to_string, "{ %" PRId64 ", ", (int64_t) row->key);

    if(row->cells != NULL)
    {
        assert(row->no_columns == 0);

        for(snode_t* node = HEAD(row->cells); node != NULL; node = NEXT(node))
        {
            if(to_string + strlen(to_string) - orig_offset > MAX_PRINT_BUFF - 10)
            {
                sprintf(to_string + strlen(to_string), "..");
                break;
            }

            db_row_t * subrow = (db_row_t *) node->value;
            long_row_to_string(subrow, to_string + strlen(to_string), len, orig_offset);
        }
    }

    if(row->no_columns > 0)
    {
        sprintf(to_string + strlen(to_string), "[ ");
        for(int i=0; i<row->no_columns; i++)
        {
            if(to_string + strlen(to_string) - orig_offset > MAX_PRINT_BUFF - 10)
            {
                sprintf(to_string + strlen(to_string), "..");
                break;
            }

#if (PRINT_BLOBS > 0)
            if(i<(row->no_columns - 1) || row->last_blob_size <= 0)
            {
                sprintf(to_string + strlen(to_string), "%" PRId64 ", ", (int64_t) row->column_array[i]);
            }
            else
            {
#if (PRINT_BLOBS_AS_LONG > 0)
                for(int bi=0;bi < row->last_blob_size / sizeof(long);bi++)
                    sprintf(to_string + strlen(to_string), "%lu ",  *((unsigned long *) row->column_array[i] + bi));
#else
                sprintf(to_string + strlen(to_string), "%s, ", (char *) row->column_array[i]);
#endif
            }

#else
            sprintf(to_string + strlen(to_string), "%" PRId64 ", ", (int64_t) row->column_array[i]);
#endif
        }
        sprintf(to_string + strlen(to_string), " ]");
    }

    sprintf(to_string + strlen(to_string), "}, ");

    *len = strlen(to_string);
}

int table_verify_cell_range_version(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
                                        int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table)
{
    assert(no_primary_keys == 1);

    snode_t * row_node = skiplist_search(table->rows, primary_keys[0]);

    if(row_node == NULL)
        return -1;

    db_row_t* row = (db_row_t *) row_node->value;

    for(int i=0;i<no_clustering_keys-1;i++)
    {
        assert(start_clustering_keys[i] == end_clustering_keys[i] && "For first N-1 clustering keys, start key must be equal to end key");

        snode_t * row_node = skiplist_search(row->cells, start_clustering_keys[i]);

        if(row_node == NULL)
            return -1;

        db_row_t* row = (db_row_t *) row_node->value;
    }

    snode_t * start_row = skiplist_search_higher(row->cells, start_clustering_keys[no_clustering_keys-1]);
    int i = 0;

    for(snode_t * cell_row_node = start_row; cell_row_node != NULL && (int64_t) cell_row_node->key < (int64_t) end_clustering_keys[no_clustering_keys-1]; cell_row_node=NEXT(cell_row_node), i++)
    {
        db_row_t* cell_row = (db_row_t *) cell_row_node->value;

        // Some keys were removed from the backend since the range query happened:
        if(i>(no_range_results - 1))
            return 1;

        if((int64_t) cell_row->key != range_result_keys[i])
            return 1;

        int cmp = compare_vc(cell_row->version, range_result_versions[i]);
        if(cmp != 0)
            return cmp;
    }

    // Some extra keys were added to the backend since the range query happened:
    if(i<no_range_results)
        return 1;

    return 0;
}


WORD* table_search_columns(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, int* column_idxs, int no_columns, db_table_t * table)
{
    db_schema_t * schema = table->schema;

    assert(no_columns > 0 && "No column indexes given");
    assert(no_clustering_keys >= schema->min_no_clustering_keys && "Not enough clustering keys given");

    db_row_t* row = table_search_clustering(primary_keys, clustering_keys, no_clustering_keys, table);

    if(row == NULL)
        return NULL;

    assert(row->column_array != NULL && row->no_columns > 0);

    WORD* results = (WORD*) malloc(no_columns * sizeof(WORD));

    for(int i=0;i<no_columns;i++)
    {
        assert(column_idxs[i] <= row->no_columns + schema->no_primary_keys + no_clustering_keys && "Column index doesn't exist in backend (DB corrupted?)");

        if(column_idxs[i] < schema->no_primary_keys)
            results[i] = primary_keys[column_idxs[i]];
        else if(column_idxs[i] < schema->no_primary_keys + no_clustering_keys)
            results[i] = clustering_keys[column_idxs[i] - schema->no_primary_keys];
        else
            results[i] = row->column_array[column_idxs[i] - schema->no_primary_keys - no_clustering_keys];
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

int table_verify_index_version(WORD index_key, int idx_idx, vector_clock * version, db_table_t * table)
{
    db_schema_t * schema = table->schema;

    assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

    snode_t * row_node = skiplist_search(table->indexes[idx_idx], index_key);

    if(row_node == NULL)
        return 1;

    return compare_vc(version, ((db_row_t *) (row_node->value))->version);
}

int table_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, db_table_t * table)
{
    db_schema_t * schema = table->schema;
    int no_results = 0;

    assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

    *start_row = skiplist_search_higher(table->indexes[idx_idx], start_idx_key);

    for(*end_row = *start_row; (*end_row != NULL) && ((int64_t) (*end_row)->key < (int64_t) end_idx_key); *end_row=NEXT(*end_row), no_results++);

    return no_results+1;
}

int table_verify_index_range_version(int idx_idx, WORD start_idx_key, WORD end_idx_key,
                                        int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table)
{
    db_schema_t * schema = table->schema;
    int i = 0;

    assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

    snode_t * start_row = skiplist_search_higher(table->indexes[idx_idx], start_idx_key);

    for(snode_t * cell_row_node = start_row; cell_row_node != NULL && (int64_t) cell_row_node->key < (int64_t) end_idx_key; cell_row_node=NEXT(cell_row_node), i++)
    {
        db_row_t* cell_row = (db_row_t *) cell_row_node->value;

        // Some keys were removed from the backend since the range query happened:
        if(i>(no_range_results - 1))
            return 1;

        if((int64_t) cell_row->key != range_result_keys[i])
            return 1;

        int cmp = compare_vc(cell_row->version, range_result_versions[i]);
        if(cmp != 0)
            return cmp;
    }

    // Some extra keys were added to the backend since the range query happened:
    if(i<no_range_results)
        return 1;

    return 0;
}

int table_delete_row(WORD* primary_keys, vector_clock * version, db_table_t * table, db_t * db, unsigned int * fastrandstate)
{
    db_row_t* row = (db_row_t *) (skiplist_delete(table->rows, primary_keys[0]));

    snode_t * exists = skiplist_search(table->row_tombstones, primary_keys[0]);

    if(exists != NULL)
        skiplist_insert(table->row_tombstones, primary_keys[0], (version != NULL)? copy_vc(version) : NULL, fastrandstate);

    if(row != NULL)
    {
        free_db_row(row, table->schema, db);
    }
    else
    {
        log_warn("table_delete_row(): Row with pk %" PRId64 " doesn't exist!", (int64_t) primary_keys[0]);
    }

    return row == NULL;
}

int table_delete_by_index(WORD index_key, int idx_idx, db_table_t * table)
{
    db_schema_t * schema = table->schema;

    assert(idx_idx <= schema->no_index_keys == 1 && "Index index out of range");

    db_row_t* row = (db_row_t *) (skiplist_delete(table->indexes[idx_idx], index_key));

    // TO DO: Re-enable this after enhancing indexes:

//  if(row != NULL)
//      free_db_row(row, table->schema);

    return row == NULL;
}


// DB API:

int db_insert_transactional(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, vector_clock * version, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
#if (VERBOSE_BACKEND > 0)
    log_info("BACKEND: db_insert_transactional: Attempting to insert %d total columns into backend:", min_no_cols);
    for(int i=0;i<min_no_cols;i++)
        log_info("column_values[%d] = %" PRId64 "", i, (int64_t) column_values[i]);
#endif

    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_insert(column_values, no_cols, no_clustering_keys, last_blob_size, version, table, fastrandstate);
}

int db_insert(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
    return db_insert_transactional(column_values, no_cols, no_clustering_keys, last_blob_size, NULL, table_key, db, fastrandstate);
}

int db_update_transactional(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, vector_clock * version, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_update(column_values, no_cols, no_clustering_keys, last_blob_size, col_idxs, version, table);
}

int db_update(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, WORD table_key, db_t * db)
{
    return db_update_transactional(column_values, no_cols, no_clustering_keys, last_blob_size, col_idxs, NULL, table_key, db);
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

int db_verify_row_range_version(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
                                    int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_verify_row_range_version(start_primary_keys, end_primary_keys, no_primary_keys,
            range_result_keys, range_result_versions, no_range_results, table);
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

int db_verify_cell_version(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, vector_clock * version, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return DB_ERR_NO_TABLE;

    db_table_t * table = (db_table_t *) (node->value);

    return table_verify_cell_version(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys, version, table);
}

int db_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_range_search_clustering(primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, start_row, end_row, table);
}

int db_verify_cell_range_version(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, WORD table_key,
                                    int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_verify_cell_range_version(primary_keys, no_primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, range_result_keys, range_result_versions, no_range_results, table);
}


WORD* db_search_columns(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, int* column_idxs, int no_columns, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return NULL;

    db_table_t * table = (db_table_t *) (node->value);

    return table_search_columns(primary_keys, clustering_keys, no_clustering_keys, column_idxs, no_columns, table);
}

db_row_t* db_search_index(WORD index_key, int idx_idx, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return NULL;

    db_table_t * table = (db_table_t *) (node->value);

    return table_search_index(index_key, idx_idx, table);
}

int db_verify_index_version(WORD index_key, int idx_idx, WORD table_key, vector_clock * version, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -2;

    db_table_t * table = (db_table_t *) (node->value);

    return table_verify_index_version(index_key, idx_idx, version, table);
}

int db_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_range_search_index(idx_idx, start_idx_key, end_idx_key, start_row, end_row, table);
}

int db_verify_index_range_version(int idx_idx, WORD start_idx_key, WORD end_idx_key,
                                    int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_verify_index_range_version(idx_idx, start_idx_key, end_idx_key, range_result_keys, range_result_versions, no_range_results, table);
}

int db_delete_row_transactional(WORD* primary_keys, vector_clock * version, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
    {
        log_warn("db_delete_row(): Table with pk %" PRId64 " doesn't exist!", (int64_t) table_key);
        return -1;
    }

    db_table_t * table = (db_table_t *) (node->value);

    return table_delete_row(primary_keys, version, table, db, fastrandstate);
}

int db_delete_row(WORD* primary_keys, WORD table_key, db_t * db, unsigned int * fastrandstate)
{
    return db_delete_row_transactional(primary_keys, NULL, table_key, db, fastrandstate);
}

int db_delete_by_index(WORD index_key, int idx_idx, WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return -1;

    db_table_t * table = (db_table_t *) (node->value);

    return table_delete_by_index(index_key, idx_idx, table);
}
