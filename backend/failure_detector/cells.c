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
 * cells.c
 *
 *      Author: aagapi
 */

#include "backend/failure_detector/cells.h"
#include "backend/log.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

// Vector Clock serialization:

int serialize_vc(vector_clock * vc, void ** buf, unsigned * len)
{
    VectorClockMessage msg = VECTOR_CLOCK_MESSAGE__INIT;
    init_vc_msg(&msg, vc);

    *len = vector_clock_message__get_packed_size (&msg);
    *buf = malloc (*len);
    vector_clock_message__pack (&msg, *buf);

    free_vc_msg(&msg);

    return 0;
}

int deserialize_vc(void * buf, unsigned msg_len, vector_clock ** vc)
{
      VectorClockMessage * msg = vector_clock_message__unpack (NULL, msg_len, buf);

      if (msg == NULL)
      { // Something failed
        log_error("error unpacking vector_clock message");
        return 1;
      }

      assert(msg->n_ids == msg->n_counters);

      *vc = init_vc_from_msg(msg);

      vector_clock_message__free_unpacked(msg, NULL);

      return 0;
}


// Cell Address:

cell_address * init_cell_address(int64_t table_key, int64_t * keys, int no_keys)
{
    cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
    ca->table_key = table_key;
    ca->keys = keys;
    ca->no_keys = no_keys;

    return ca;
}

cell_address * init_cell_address_copy(int64_t table_key, int64_t * keys, int no_keys)
{
    cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
    ca->table_key = table_key;
    ca->no_keys = no_keys;
    ca->keys = (int64_t *) malloc(no_keys * sizeof(int64_t));
    for(int i=0;i<no_keys;i++)
        ca->keys[i] = keys[i];

    return ca;
}

cell_address * init_cell_address_copy2(int64_t table_key, int64_t * primary_keys, int no_primary_keys, int64_t * clustering_keys, int no_clustering_keys)
{
    int i = 0;
    cell_address * c = (cell_address *) malloc(sizeof(cell_address));
    c->table_key = (int64_t) table_key;
    c->no_keys = no_primary_keys + no_clustering_keys;

    assert(c->no_keys > 0);

    c->keys = (int64_t *) malloc(c->no_keys * sizeof(int64_t));
    for(;i<no_primary_keys;i++)
        c->keys[i] = (int64_t) primary_keys[i];

    for(;i<c->no_keys;i++)
        c->keys[i] = (int64_t) clustering_keys[i-no_primary_keys];

    return c;
}

cell_address * init_cell_address_single_key_copy(int64_t table_key, int64_t key)
{
    cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
    ca->table_key = table_key;
    ca->no_keys = 1;
    ca->keys = (int64_t *) malloc(sizeof(int64_t));
    ca->keys[0] = key;

    return ca;
}

int copy_cell_address(cell_address * ca, int64_t table_key, int64_t * keys, int no_keys)
{
    ca->table_key = table_key;
    ca->keys = keys;
    ca->no_keys = no_keys;

    return 0;
}


void free_cell_address(cell_address * ca)
{
    free(ca->keys);
    free(ca);
}

void init_cell_address_msg(CellAddressMessage * msg, cell_address * ca)
{
    msg->table_key = ca->table_key;
    msg->n_keys = ca->no_keys;
    msg->keys = (int64_t *) malloc(ca->no_keys * sizeof(int64_t));
    for(int i=0;i<ca->no_keys;i++)
        msg->keys[i] = ca->keys[i];
}

cell_address * init_cell_address_from_msg(CellAddressMessage * msg)
{
    return init_cell_address_copy(msg->table_key, msg->keys, msg->n_keys);
}

void free_cell_address_msg(CellAddressMessage * msg)
{
    free(msg->keys);
}

int serialize_cell_address(cell_address * ca, void ** buf, unsigned * len)
{
    CellAddressMessage msg = CELL_ADDRESS_MESSAGE__INIT;
    init_cell_address_msg(&msg, ca);

    *len = cell_address_message__get_packed_size (&msg);
    *buf = malloc (*len);
    cell_address_message__pack (&msg, *buf);

    free_cell_address_msg(&msg);

    return 0;
}

int deserialize_cell_address(void * buf, unsigned msg_len, cell_address ** ca)
{
    CellAddressMessage * msg = cell_address_message__unpack (NULL, msg_len, buf);

    if (msg == NULL)
    {
        log_error("error unpacking cell_address message");
        return 1;
    }

    *ca = init_cell_address_from_msg(msg);

    cell_address_message__free_unpacked(msg, NULL);

    return 0;
}

int equals_cell_address(cell_address * ca1, cell_address * ca2)
{
    if((ca1 != NULL && ca2 == NULL) || (ca1 == NULL && ca2 != NULL))
        return 0;

    if(ca1->table_key != ca2->table_key || ca1->no_keys != ca2->no_keys)
        return 0;

    for(int i=0;i<ca1->no_keys;i++)
        if(ca1->keys[i] != ca2->keys[i])
            return 0;

    return 1;
}

char * to_string_cell_address(cell_address * ca, char * msg_buff)
{
    char * crt_ptr = msg_buff;

    sprintf(crt_ptr, "CellAddress(table_key=%" PRId64 ", keys={", ca->table_key);
    crt_ptr += strlen(crt_ptr);

    for(int i=0;i<ca->no_keys;i++)
    {
        sprintf(crt_ptr, "%" PRId64 ", ",  ca->keys[i]);
        crt_ptr += strlen(crt_ptr);
    }

    sprintf(crt_ptr, "})");

    return msg_buff;
}

// Cell:

cell * init_cell(int64_t table_key, int64_t * keys, int no_keys, int64_t * columns, int no_columns, WORD last_blob, size_t last_blob_size, vector_clock * version)
{
    cell * ca = (cell *) malloc(sizeof(cell));
    ca->table_key = table_key;
    ca->keys = keys;
    ca->columns = columns;
    ca->no_keys = no_keys;
    ca->no_columns = no_columns;
    ca->last_blob = last_blob;
    ca->last_blob_size = last_blob_size;
    ca->version = version;

    return ca;
}

void copy_cell(cell * ca, int64_t table_key, int64_t * keys, int no_keys, int64_t * columns, int no_columns, WORD last_blob, size_t last_blob_size, vector_clock * version)
{
    ca->table_key = table_key;

    ca->no_keys = no_keys;
    ca->keys = (int64_t *) malloc(no_keys * sizeof(int64_t));
    for(int i=0;i<no_keys;i++)
        ca->keys[i] = keys[i];

    assert(last_blob == NULL || last_blob_size > 0);

    ca->no_columns = no_columns;
    ca->columns = (int64_t *) malloc(no_columns * sizeof(int64_t));
    for(int i=0;i<no_columns;i++)
        ca->columns[i] = columns[i];

    ca->last_blob_size = last_blob_size;
    if(last_blob != NULL)
    {
        ca->last_blob = malloc(last_blob_size);
        memcpy(ca->last_blob, last_blob, last_blob_size);
    }
    else
    {
        ca->last_blob = NULL;
    }

    if(version != NULL)
        ca->version = copy_vc(version);
    else
        ca->version = NULL;
}

cell * init_cell_copy(int64_t table_key, int64_t * keys, int no_keys, int64_t * columns, int no_columns, WORD last_blob, size_t last_blob_size, vector_clock * version)
{
    cell * ca = (cell *) malloc(sizeof(cell));
    copy_cell(ca, table_key, keys, no_keys, columns, no_columns, last_blob, last_blob_size, version);
    return ca;
}

cell_address * get_cell_address(cell * c)
{
    return init_cell_address_copy(c->table_key, c->keys, c->no_keys);
}

void free_cell_ptrs(cell * ca)
{
    if(ca->keys != NULL)
        free(ca->keys);

    if(ca->columns != NULL)
    {
        free(ca->columns);
    }

    if(ca->last_blob != NULL)
    {
        assert(ca->last_blob_size > 0);
        free(ca->last_blob);
    }

    free_vc(ca->version);
}

void free_cell(cell * ca)
{
    free_cell_ptrs(ca);
    free(ca);
}

void init_cell_msg(VersionedCellMessage * msg, cell * ca, VectorClockMessage * vc_msg)
{
    msg->table_key = ca->table_key;
    msg->n_keys = ca->no_keys;
    msg->keys = (int64_t *) malloc(ca->no_keys * sizeof(int64_t));
    for(int i=0;i<ca->no_keys;i++)
        msg->keys[i] = ca->keys[i];

    msg->n_columns = ca->no_columns;
    if(ca->no_columns > 0)
        msg->columns = (int64_t *) malloc(ca->no_columns * sizeof(int64_t));
    for(int i=0;i<ca->no_columns;i++)
        msg->columns[i] = ca->columns[i];

    if(ca->last_blob != NULL)
    {
        assert(ca->last_blob_size > 0);
        msg->blob.len = ca->last_blob_size;
        msg->blob.data = malloc(ca->last_blob_size);
        memcpy(msg->blob.data, ca->last_blob, ca->last_blob_size);
    }
    else
    {
        msg->blob.data = NULL;
        msg->blob.len = 0;
    }

    if(ca->version != NULL)
    {
        init_vc_msg(vc_msg, ca->version);
        msg->version = vc_msg;
    }
    else
    {
//      msg->version = NULL;
    }
}

cell * copy_cell_from_msg(cell * c, VersionedCellMessage * msg)
{
    copy_cell(c, msg->table_key, msg->keys, msg->n_keys, msg->columns, msg->n_columns, msg->blob.data, msg->blob.len, (msg->version != NULL)?(init_vc_from_msg(msg->version)):(NULL));
    return c;
}

cell * init_cell_from_msg(VersionedCellMessage * msg)
{
    if(msg == NULL)
        return NULL;

    vector_clock * vc = NULL;
    if(msg->version != NULL)
        vc = init_vc_from_msg(msg->version);
    cell * c = init_cell_copy(msg->table_key, msg->keys, msg->n_keys, msg->columns, msg->n_columns, msg->blob.data, msg->blob.len, vc);

    return c;
}

void free_cell_msg(VersionedCellMessage * msg)
{
    if(msg->keys != NULL)
        free(msg->keys);
    if(msg->columns != NULL)
        free(msg->columns);
    if(msg->blob.data != NULL && msg->blob.len > 0)
        free(msg->blob.data);
    if(msg->version != NULL)
        free_vc_msg(msg->version);
}

int serialize_cell(cell * ca, void ** buf, unsigned * len)
{
    VersionedCellMessage msg = VERSIONED_CELL_MESSAGE__INIT;
    VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;

    init_cell_msg(&msg, ca, &vc_msg);

    *len = versioned_cell_message__get_packed_size (&msg);
    *buf = malloc (*len);
    versioned_cell_message__pack (&msg, *buf);

    free_cell_msg(&msg);

    return 0;
}

int deserialize_cell(void * buf, unsigned msg_len, cell ** ca)
{
    VersionedCellMessage * msg = versioned_cell_message__unpack (NULL, msg_len, buf);

    if (msg == NULL)
    {
        log_error("error unpacking cell message");
        return 1;
    }

    *ca = init_cell_from_msg(msg);

    versioned_cell_message__free_unpacked(msg, NULL);

    return 0;
}

int equals_cell(cell * ca1, cell * ca2)
{
    if(ca1 == NULL && ca2 == NULL)
        return 1;

    if(ca1 != NULL && ca2 == NULL)
        return 0;

    if(ca1 == NULL && ca2 != NULL)
        return 0;

    if(ca1->table_key != ca2->table_key || ca1->no_keys != ca2->no_keys)
        return 0;

    for(int i=0;i<ca1->no_keys;i++)
        if(ca1->keys[i] != ca2->keys[i])
            return 0;

    for(int i=0;i<ca1->no_columns;i++)
        if(ca1->columns[i] != ca2->columns[i])
            return 0;

    if(ca1->last_blob_size != ca2->last_blob_size)
        return 0;
    if(ca1->last_blob_size > 0)
    {
        assert(ca1->last_blob != NULL && ca2->last_blob != NULL);

        if(memcmp(ca1->last_blob, ca2->last_blob, ca1->last_blob_size) != 0)
            return 0;
    }

    if(compare_vc(ca1->version, ca2->version))
        return 0;

    return 1;
}

char * to_string_cell(cell * ca, char * msg_buff)
{
    char * crt_ptr = msg_buff;

    sprintf(crt_ptr, "Cell(table_key=%" PRId64 ", keys={", ca->table_key);
    crt_ptr += strlen(crt_ptr);

    for(int i=0;i<ca->no_keys;i++)
    {
        sprintf(crt_ptr, "%" PRId64 ", ", ca->keys[i]);
        crt_ptr += strlen(crt_ptr);
    }

    sprintf(crt_ptr, "}, columns={");
    crt_ptr += strlen(crt_ptr);

    for(int i=0;i<ca->no_columns;i++)
    {
        sprintf(crt_ptr, "%" PRId64 ", ", ca->columns[i]);
        crt_ptr += strlen(crt_ptr);
    }

    sprintf(crt_ptr, "}, version=");
    crt_ptr += strlen(crt_ptr);

    if(ca->version != NULL)
    {
        to_string_vc(ca->version, crt_ptr);
        crt_ptr += strlen(crt_ptr);
    }

    sprintf(crt_ptr, ")");

    return msg_buff;
}


