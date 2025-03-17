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
 * db_messages_test.c
 *
 *      Author: aagapi
 */

#include "backend/failure_detector/vector_clock.h"
#include "backend/failure_detector/fd.h"
#include "backend/failure_detector/cells.h"
#include "backend/failure_detector/db_queries.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <uuid/uuid.h>
#include <string.h>

#define MAX_MSG_SIZE_VC 1024

int write_msg_to_file (unsigned char *buff, unsigned len, FILE * fp)
{
    unsigned written = fwrite(buff, len, 1, fp);

//  printf("Wrote %d / %d bytes to file\n", written * len, len);

    return (written == 1)? 0 : -1;
}

int read_msg_from_file (unsigned max_length, unsigned char *buff, FILE * fp)
{
  unsigned cur_len = 0;
  unsigned nread;

  while ((nread=fread(buff + cur_len, 1, max_length - cur_len, fp)) != 0)
  {
    printf("Read %d bytes\n", nread);
    cur_len += nread;
    if (cur_len == max_length)
    {
      fprintf(stderr, "max message length exceeded\n");
      return -1;
    }
  }
  return cur_len;
}

void write_read_from_file(void * buf_w, unsigned len_w, unsigned char *buf_r, unsigned * len_r)
{
    // Serialize it to file:

    FILE * fptr_w = fopen("/tmp/vc.test","wb");
    int success = write_msg_to_file(buf_w, len_w, fptr_w);
    assert(success == 0);
    fclose(fptr_w);

    // Read it back:

    FILE * fptr_r = fopen("/tmp/vc.test","rb");

    *len_r = read_msg_from_file(MAX_MSG_SIZE_VC, buf_r, fptr_r);

    if(*len_r != len_w)
    {
        printf("len_r=%d != len_w=%d\n", *len_r, len_w);
        assert(0);
    }
    fclose(fptr_r);
}


int main (int argc, const char * argv[])
{
    FILE *fptr_w = NULL, * fptr_r = NULL;
    void * buf_w;
    unsigned len_w;
    unsigned char buf_r[MAX_MSG_SIZE_VC];
//  void * buf_r = malloc(MAX_MSG_SIZE_VC);
    unsigned len_r;
    char err_msg[1024], err_msg2[1024];

    // Generate a dummy VC message:

    int node_ids[] = {0,1};
    int64_t counters[] = {0,0};

    vector_clock * vc = init_vc(2, node_ids, counters, 1), * vc_r = NULL;
    add_component_vc(vc, 2, 0);
    increment_vc(vc, 0);
    increment_vc(vc, 0);
    increment_vc(vc, 1);
    increment_vc(vc, 2);
    increment_vc(vc, 2);
    serialize_vc(vc, &buf_w, &len_w);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_vc(buf_r, len_r, &vc_r);
    printf("VC message: %s\n", to_string_vc(vc, err_msg));
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Generate dummy GS message:

    gossip_state * gs = init_gossip_state(0, 0, 0, 0, "localhost", 32000, vc), * gs_r = NULL;
    serialize_gs(gs, &buf_w, &len_w);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_gs(buf_r, len_r, &gs_r);

    printf("GossipState message: %s\n", to_string_gs(gs, err_msg));
    if(!equals_gs(gs, gs_r))
    {
        printf("GS read mismatch (%s)!\n", to_string_gs(gs_r, err_msg));
        assert(0);
    }

    // Generate dummy Membership State message:

    node_description * nds = (node_description *) malloc(3 * sizeof(node_description));
    copy_node_description(&nds[0], 0, 0, 0, 0, "localhost", 32000);
    copy_node_description(&nds[1], 0, 1, 0, 0, "localhost", 32001);
    copy_node_description(&nds[2], 1, 2, 0, 0, "localhost", 32002);

    node_description * client_nds = (node_description *) malloc(1 * sizeof(node_description));
    copy_node_description(&client_nds[0], 0, 0, 0, 0, "localhost", 22000);

    membership_state * ms = init_membership_state(3, nds, 1, client_nds, vc), * ms_r = NULL;
    serialize_membership_state(ms, &buf_w, &len_w);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_membership_state(buf_r, len_r, &ms_r);

    printf("MembershipState message: %s\n", to_string_membership_state(ms, err_msg));
    if(!equals_membership_state(ms, ms_r))
    {
        printf("MembershipState read mismatch (%s)!\n", to_string_membership_state(ms_r, err_msg));
        assert(0);
    }

    // Generate dummy Membership Agreement message:

    membership_agreement_msg * ma = init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_PROPOSE, 0, ms, 0, vc), * ma_r = NULL;
    serialize_membership_agreement_msg(ma, &buf_w, &len_w);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    unsigned msg_len = ((unsigned *) buf_r)[0];
    deserialize_membership_agreement_msg(buf_r + sizeof(int), msg_len, &ma_r);

    printf("MembershipAgreement message: %s\n", to_string_membership_agreement_msg(ma, err_msg));
    if(!equals_membership_agreement_msg(ma, ma_r))
    {
        printf("MembershipAgreement read mismatch (%s)!\n", to_string_membership_agreement_msg(ma_r, err_msg));
        assert(0);
    }

    // Generate a dummy Cell and CellAddress:

    int64_t key = 1, end_key = 3;
    int64_t column = 1, end_column = 5;
    uuid_t txnid;
    uuid_generate(txnid);

    int no_cells = 2;
    cell * cll = (cell *) malloc(no_cells * sizeof(cell));
    for(int i=0;i<no_cells;i++)
        copy_cell(cll + i, 0, (i==0)?(&key) : (&end_key), 1, (i==0)?(&column) : (&end_column), 1, NULL, 0, vc);
    cell_address * cell_address = init_cell_address(0, &key, 1), * end_cell_address = init_cell_address(0, &end_key, 1);

    // Generate dummy Write Query:

    short mtype = -1;
    write_query * wquery = init_write_query(cll, RPC_TYPE_WRITE, &txnid, 3), * wquery_r = NULL;
    serialize_write_query(wquery, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    assert(((int *) buf_r)[0] == len_r - sizeof(int));
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &wquery_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_WRITE);

    printf("Write query: %s (%s)\n", to_string_write_query(wquery, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_write_query(wquery, wquery_r))
    {
        printf("Write query read mismatch (%s)!\n", to_string_write_query(wquery_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }


    // Generate dummy Read Query:

    read_query * rquery = init_read_query(cell_address, &txnid, 3), * rquery_r = NULL;
    serialize_read_query(rquery, &buf_w, &len_w, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &rquery_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_READ);

    printf("Read query: %s (%s)\n", to_string_read_query(rquery, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_read_query(rquery, rquery_r))
    {
        printf("Read query read mismatch (%s)!\n", to_string_read_query(rquery_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Ack Message:

    ack_message * am = init_ack_message(cell_address, 1, &txnid, 3), * am_r = NULL;
    serialize_ack_message(am, &buf_w, &len_w, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_client_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &am_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_ACK);

    printf("Ack message: %s (%s)\n", to_string_ack_message(am, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_ack_message(am, am_r))
    {
        printf("Ack message read mismatch (%s)!\n", to_string_ack_message(am_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Range read query:

    range_read_query * rrq = init_range_read_query(cell_address, end_cell_address, &txnid, 3), * rrq_r = NULL;
    serialize_range_read_query(rrq, &buf_w, &len_w, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &rrq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_RANGE_READ);

    printf("Range Read Query: %s (%s)\n", to_string_range_read_query(rrq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_range_read_query(rrq, rrq_r))
    {
        printf("Range Read Query read mismatch (%s)!\n", to_string_range_read_query(rrq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Range read response:

    range_read_response_message * rrm = init_range_read_response_message(cll, no_cells, &txnid, 3), * rrm_r = NULL;
    serialize_range_read_response_message(rrm, &buf_w, &len_w, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_client_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &rrm_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_RANGE_READ_RESPONSE);

    printf("Range Read Response: %s (%s)\n", to_string_range_read_response_message(rrm, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_range_read_response_message(rrm, rrm_r))
    {
        printf("Range Read Response read mismatch (%s)!\n", to_string_range_read_response_message(rrm_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Create queue message:

    queue_query_message * cq = init_create_queue_message(cell_address, &txnid, 3), * cq_r = NULL;
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Create Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Create Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Delete queue message:

    cq = init_delete_queue_message(cell_address, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Delete Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Delete Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Subscribe queue message:

    cq = init_subscribe_queue_message(cell_address, 1, 2, 3, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Subscribe Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Subscribe Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Unsubscribe queue message:

    cq = init_unsubscribe_queue_message(cell_address, 1, 2, 3, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Unsubscribe Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Unsubscribe Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Enqueue message:

    cq = init_enqueue_message(cell_address, cll, no_cells, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Enqueue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Enqueue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Read queue message:

    cq = init_read_queue_message(cell_address, 1, 2, 3, 2, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Read Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Read Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Consume queue message:

    cq = init_consume_queue_message(cell_address, 1, 2, 3, 1, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Consume Queue: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Consume Queue read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Read queue response message:

    cq = init_read_queue_response(cell_address, cll, no_cells, 1, 2, 3, 1, 2, 1, &txnid, 3);
    serialize_queue_message(cq, &buf_w, &len_w, 0, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_client_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &cq_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_QUEUE);

    printf("Read Queue Response: %s (%s)\n", to_string_queue_message(cq, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_queue_message(cq, cq_r))
    {
        printf("Read Queue Response read mismatch (%s)!\n", to_string_queue_message(cq_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }

    // Txn message:

    txn_message * tm = init_txn_message(DB_TXN_VALIDATION,
                                    cll, 2,
                                    cll, 2,
                                    cll, 2,
                                    cll, 2,
                                    &txnid, vc, 3), * tm_r = NULL;
    serialize_txn_message(tm, &buf_w, &len_w, 1, vc);
    write_read_from_file(buf_w, len_w, buf_r, &len_r);
    deserialize_server_message(((int *) buf_r + 1), len_r - sizeof(int), (void *) &tm_r, &mtype, &vc_r);
    assert(mtype == RPC_TYPE_TXN);

    printf("Txn Message: %s (%s)\n", to_string_txn_message(tm, err_msg), to_string_vc(vc_r, err_msg2));
    if(!equals_txn_message(tm, tm_r))
    {
        printf("Txn Message read mismatch (%s)!\n", to_string_txn_message(tm_r, err_msg));
        assert(0);
    }
    if(compare_vc(vc, vc_r) != 0)
    {
        printf("VC read mismatch (%s)!\n", to_string_vc(vc_r, err_msg));
        assert(0);
    }
}





