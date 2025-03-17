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
 * fd.c
 * Author: aagapi
 */

#include "backend/failure_detector/fd.h"
#include "backend/failure_detector/db_messages.pb-c.h"
#include "backend/log.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* Node description: */

void init_ns_msg_from_description(NodeStateMessage * ns_msg, node_description * nd)
{
    ns_msg->status = nd->status;
    ns_msg->node_id = nd->node_id;
    ns_msg->rack_id = nd->rack_id;
    ns_msg->dc_id = nd->dc_id;
    ns_msg->port = nd->portno;

    assert(nd->hostname != NULL);

    ns_msg->hostname.len = strnlen(nd->hostname, 256) + 1;
    ns_msg->hostname.data = malloc(ns_msg->hostname.len);
    memcpy(ns_msg->hostname.data, nd->hostname, ns_msg->hostname.len);
}

int copy_node_description(node_description * nd, int status, int node_id, int rack_id, int dc_id, char * hostname, unsigned short portno)
{
    nd->status = status;
    nd->node_id = node_id;
    nd->rack_id = rack_id;
    nd->dc_id = dc_id;
    nd->portno = portno;

    bzero((void *) &nd->address, sizeof(struct sockaddr_in));
    nd->hostname = NULL;

    if(hostname != NULL)
    {
        struct hostent * host = gethostbyname(hostname);
        if (host == NULL)
        {
            log_error("ERROR, no such host %s", hostname);
            assert(0);
            return -1;
        }

        nd->address.sin_family = AF_INET;
        bcopy((char *)host->h_addr, (char *)&(nd->address.sin_addr.s_addr), host->h_length);
        nd->address.sin_port = htons(portno);

        nd->hostname = strndup(hostname, strnlen(hostname, 256) + 1);
    }

    return 0;
}

node_description * init_node_description(int status, int node_id, int rack_id, int dc_id, char * hostname, unsigned short portno)
{
    node_description * nd = (node_description *) malloc(sizeof(node_description));

    copy_node_description(nd, status, node_id, rack_id, dc_id, hostname, portno);

    return nd;
}

void free_node_description(node_description * nd)
{
    if(nd->hostname != NULL)
        free(nd->hostname);
    free(nd);
}

int equals_node_description(node_description * nd1, node_description * nd2)
{
    return nd1->node_id == nd2->node_id && nd1->rack_id == nd2->rack_id && nd1->dc_id == nd2->dc_id;
}

char * to_string_node_description(node_description * nd, char * msg_buff)
{
    sprintf(msg_buff, "Node(status=%d, node_id=%d, rack_id=%d, dc_id=%d, hostname=%s, port=%d)", nd->status, nd->node_id, nd->rack_id, nd->dc_id, (nd->hostname != NULL)?(nd->hostname):"NULL", nd->portno);
    return msg_buff;
}


/* Gossip state: */

gossip_state * init_gossip_state(int status, int node_id, int rack_id, int dc_id, char * hostname, unsigned short portno, vector_clock * vc)
{
    gossip_state * gs = (gossip_state *) malloc(sizeof(struct gossip_state));
    bzero(gs, sizeof(struct gossip_state));
    copy_node_description(&(gs->nd), status, node_id, rack_id, dc_id, hostname, portno);
    gs->vc = (vc != NULL)?vc:init_vc(0, NULL, NULL, 0);

    return gs;
}

void free_gossip_state(gossip_state * gs)
{
    free_vc(gs->vc);
    free(gs);
}

void free_gossip_msg(GossipMessage * msg)
{
    free_vc_msg(msg->vc);
}

void init_ns_msg(NodeStateMessage * ns_msg, gossip_state * gs)
{
    init_ns_msg_from_description(ns_msg, &(gs->nd));
}

int serialize_gs(gossip_state * gs, void ** buf, unsigned * len)
{
    GossipMessage msg = GOSSIP_MESSAGE__INIT;

    VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;
    init_vc_msg(&vc_msg, gs->vc);

    NodeStateMessage ns_msg = NODE_STATE_MESSAGE__INIT;
    init_ns_msg(&ns_msg, gs);

    msg.vc = &vc_msg;
    msg.node_state = &ns_msg;

    *len = gossip_message__get_packed_size (&msg);
    *buf = malloc (*len);
    gossip_message__pack (&msg, *buf);

    free_gossip_msg(&msg);

    return 0;
}

int deserialize_gs(void * buf, unsigned msg_len, gossip_state ** gs)
{
      GossipMessage * msg = gossip_message__unpack(NULL, msg_len, buf);

      if (msg == NULL)
      { // Something failed
        log_error("error unpacking gossip_state message");
        return 1;
      }

      vector_clock * vc = init_vc_from_msg(msg->vc);

      *gs = init_gossip_state(msg->node_state->status, msg->node_state->node_id, msg->node_state->rack_id, msg->node_state->dc_id, (char *) msg->node_state->hostname.data, msg->node_state->port, vc);

      return 0;
}

int equals_gs(gossip_state * gs1, gossip_state * gs2)
{
    return equals_node_description(&(gs1->nd), &(gs2->nd)) &&
            gs1->nd.status == gs2->nd.status &&
            compare_vc(gs1->vc, gs2->vc) == 0;
}

char * to_string_gs(gossip_state * gs, char * msg_buff)
{
    sprintf(msg_buff, "GS(node_description=");
    to_string_node_description(&(gs->nd), msg_buff + strlen(msg_buff));
    sprintf(msg_buff, ", vc=");
    to_string_vc(gs->vc, msg_buff + strlen(msg_buff));
    sprintf(msg_buff + strlen(msg_buff), ")");

    return msg_buff;
}

/* Membership: */

membership_state * init_membership_state(int no_nodes, node_description * membership, int no_client_nodes, node_description * client_membership, vector_clock * view_id)
{
    membership_state * ms = (membership_state *) malloc(sizeof(membership_state));
    ms->no_nodes = no_nodes;
    ms->membership = membership;
    ms->no_client_nodes = no_client_nodes;
    ms->client_membership = client_membership;
    ms->view_id = view_id;
    return ms;
}

membership_state * clone_membership(membership_state * m)
{
    membership_state * ms = (membership_state *) malloc(sizeof(membership_state));
    ms->no_nodes = m->no_nodes;
    ms->membership = (node_description *) malloc(m->no_nodes * sizeof(node_description));
    for(int i=0;i<m->no_nodes;i++)
        copy_node_description(ms->membership + i, m->membership[i].status, m->membership[i].node_id, m->membership[i].rack_id, m->membership[i].dc_id, m->membership[i].hostname, m->membership[i].portno);

    ms->no_client_nodes = m->no_client_nodes;
    ms->client_membership = (node_description *) malloc(m->no_client_nodes * sizeof(node_description));
    for(int i=0;i<m->no_client_nodes;i++)
        copy_node_description(ms->client_membership + i, m->client_membership[i].status, m->client_membership[i].node_id, m->client_membership[i].rack_id, m->client_membership[i].dc_id, m->client_membership[i].hostname, m->client_membership[i].portno);

    ms->view_id = (m->view_id != NULL)?(copy_vc(m->view_id)):(NULL);

    return ms;
}

void free_membership_state(membership_state * ms, int do_free_vc)
{
    free(ms->membership);
    free(ms->client_membership);
    if(do_free_vc)
        free_vc(ms->view_id);
    free(ms);
}

void free_membership_msg(MembershipViewMessage * msg)
{
    free_vc_msg(msg->view_id);

    for(int i=0;i<msg->n_membership;i++)
        free(msg->membership[i]);

    free(msg->membership);

    for(int i=0;i<msg->n_client_membership;i++)
        free(msg->client_membership[i]);

    free(msg->client_membership);
}

void init_membership_msg(MembershipViewMessage * msg, membership_state * m, VectorClockMessage * view_id_msg)
{
    NodeStateMessage **membership_v = (NodeStateMessage **) malloc (m->no_nodes * sizeof (NodeStateMessage*));
    NodeStateMessage **client_membership_v = NULL;
    if(m->no_client_nodes > 0)
        client_membership_v = (NodeStateMessage **) malloc (m->no_client_nodes * sizeof (NodeStateMessage*));

    for(int i = 0; i < m->no_nodes; i++)
    {
        membership_v[i] = malloc (sizeof (NodeStateMessage));
        node_state_message__init(membership_v[i]);
        init_ns_msg_from_description(membership_v[i], m->membership+i);
    }
    msg->n_membership = m->no_nodes;
    msg->membership = membership_v;

    for(int i = 0; i < m->no_client_nodes; i++)
    {
        client_membership_v[i] = malloc (sizeof (NodeStateMessage));
        node_state_message__init(client_membership_v[i]);
        init_ns_msg_from_description(client_membership_v[i], m->client_membership+i);
    }
    msg->n_client_membership = m->no_client_nodes;
    msg->client_membership = client_membership_v;

    msg->view_id = view_id_msg;
}

int serialize_membership_state(membership_state * m, void ** buf, unsigned * len)
{
    MembershipViewMessage msg = MEMBERSHIP_VIEW_MESSAGE__INIT;
    VectorClockMessage view_id_msg = VECTOR_CLOCK_MESSAGE__INIT;
    init_vc_msg(&view_id_msg, m->view_id);

    init_membership_msg(&msg, m, &view_id_msg);

    *len = membership_view_message__get_packed_size (&msg);
    *buf = malloc (*len);
    membership_view_message__pack (&msg, *buf);

    free_membership_msg(&msg);

    return 0;

}

membership_state * init_membership_from_msg(MembershipViewMessage * msg)
{
    vector_clock * view_id = init_vc_from_msg(msg->view_id);

    node_description * membership = (node_description *) malloc(msg->n_membership * sizeof(node_description));
    for(int i=0;i<msg->n_membership;i++)
        copy_node_description(membership+i, msg->membership[i]->status, msg->membership[i]->node_id, msg->membership[i]->rack_id, msg->membership[i]->dc_id,
                                (char *) msg->membership[i]->hostname.data, (unsigned short) msg->membership[i]->port);

    node_description * client_membership = NULL;
    if(msg->n_client_membership > 0)
    {
        client_membership = (node_description *) malloc(msg->n_client_membership * sizeof(node_description));
        for(int i=0;i<msg->n_client_membership;i++)
            copy_node_description(client_membership+i, msg->client_membership[i]->status, msg->client_membership[i]->node_id, msg->client_membership[i]->rack_id, msg->client_membership[i]->dc_id,
                                    (char *) msg->client_membership[i]->hostname.data, (unsigned short) msg->client_membership[i]->port);
    }

    return init_membership_state(msg->n_membership, membership, msg->n_client_membership, client_membership, view_id);
}

int deserialize_membership_state(void * buf, unsigned msg_len, membership_state ** ms)
{
    MembershipViewMessage * msg = membership_view_message__unpack(NULL, msg_len, buf);

    if (msg == NULL)
    {
        log_error("error unpacking membership view message");
        return 1;
    }

    *ms = init_membership_from_msg(msg);

    return 0;
}

int equals_membership_state(membership_state * gs1, membership_state * gs2)
{
    if(gs1->no_nodes != gs2->no_nodes)
        return 0;

    for(int i=0;i<gs1->no_nodes;i++)
        if(!equals_node_description(gs1->membership + i, gs2->membership + i))
            return 0;

    if(gs1->no_client_nodes != gs2->no_client_nodes)
        return 0;

    for(int i=0;i<gs1->no_client_nodes;i++)
        if(!equals_node_description(gs1->client_membership + i, gs2->client_membership + i))
            return 0;

    return 1;
}

char * to_string_membership_state(membership_state * gs, char * msg_buff)
{
    char * crt_ptr = msg_buff;
    sprintf(crt_ptr, "Membership(");
    crt_ptr += strlen(crt_ptr);

    for(int i=0;i<gs->no_nodes;i++)
    {
        to_string_node_description(gs->membership+i, crt_ptr);
        crt_ptr += strlen(crt_ptr);
        sprintf(crt_ptr, ", ");
        crt_ptr += 2;
    }

    sprintf(crt_ptr, "), Client Membership(");
    crt_ptr += strlen("), Client Membership(");

    for(int i=0;i<gs->no_client_nodes;i++)
    {
        to_string_node_description(gs->client_membership+i, crt_ptr);
        crt_ptr += strlen(crt_ptr);
        sprintf(crt_ptr, ", ");
        crt_ptr += 2;
    }

    return msg_buff;
}


/* Membership agreement messages: */

membership_agreement_msg * init_membership_agreement_msg(int msg_type, int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc)
{
    membership_agreement_msg * ma = (membership_agreement_msg *) malloc(sizeof(membership_agreement_msg));
    ma->msg_type = msg_type;
    ma->ack_status = ack_status;
    ma->membership = membership;
    ma->nonce = nonce;
    ma->vc = vc;
    return ma;
}

membership_agreement_msg * get_membership_propose_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc)
{
    return init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_PROPOSE, ack_status, membership, nonce, vc);
}

membership_agreement_msg * get_membership_response_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc)
{
    return init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_RESPONSE, ack_status, membership, nonce, vc);
}

membership_agreement_msg * get_membership_notify_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc)
{
    return init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_NOTIFY, ack_status, membership, nonce, vc);
}

membership_agreement_msg * get_membership_notify_ack_msg(int ack_status, int64_t nonce, vector_clock * vc)
{
    return init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_NOTIFY_ACK, ack_status, NULL, nonce, vc);
}

membership_agreement_msg * get_membership_join_msg(int status, int rack_id, int dc_id, char * hostname, unsigned short portno, int64_t nonce, vector_clock * vc)
{
    node_description * nd = init_node_description(status, -1, rack_id, dc_id, hostname, portno);

    nd->node_id = get_node_id((struct sockaddr *) &(nd->address));

    membership_state * membership = init_membership_state(1, nd, 0, NULL, copy_vc(vc));

    return init_membership_agreement_msg(MEMBERSHIP_AGREEMENT_JOIN, status, membership, nonce, vc);
}

void free_membership_agreement(membership_agreement_msg * ma)
{
    if(ma->membership != NULL)
        free_membership_state(ma->membership, 1);
    if(ma->vc != NULL)
        free_vc(ma->vc);
    free(ma);
}

void free_membership_agreement_msg(MembershipAgreementMessage * msg)
{
    if(msg->vc != NULL)
        free_vc_msg(msg->vc);

    if(msg->view != NULL)
        free_membership_msg(msg->view);
}

int serialize_membership_agreement_msg(membership_agreement_msg * ma, void ** buf, unsigned * len)
{
    MembershipAgreementMessage msg = MEMBERSHIP_AGREEMENT_MESSAGE__INIT;

    VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;
    init_vc_msg(&vc_msg, ma->vc);
    msg.vc = &vc_msg;

    if(ma->membership != NULL)
    {
        MembershipViewMessage mview_msg = MEMBERSHIP_VIEW_MESSAGE__INIT;
        VectorClockMessage view_id_msg = VECTOR_CLOCK_MESSAGE__INIT;
        init_vc_msg(&view_id_msg, ma->membership->view_id);
        init_membership_msg(&mview_msg, ma->membership, &view_id_msg);
        msg.view = &mview_msg;
    }
    else
    {
        msg.view = NULL;
    }

    msg.msg_type = ma->msg_type;
    msg.ack_status = ma->ack_status;
    msg.nonce = ma->nonce;

    *len = membership_agreement_message__get_packed_size (&msg);
    *len = (*len) + sizeof(int);
    *buf = malloc (*len);
    memset(*buf, 0 , *len);
    *((int *)(*buf)) = (*len) - sizeof(int);
    membership_agreement_message__pack (&msg, (void *) ((int *)(*buf) + 1));
    free_membership_agreement_msg(&msg);

    return 0;
}

int deserialize_membership_agreement_msg(void * buf, unsigned msg_len, membership_agreement_msg ** ma)
{
    MembershipAgreementMessage * msg = membership_agreement_message__unpack(NULL, msg_len, buf);

    if (msg == NULL)
    {
        log_error("error unpacking membership agreement message");
        return 1;
    }

    vector_clock * vc = init_vc_from_msg(msg->vc);
    membership_state * membership = (msg->view != NULL)?(init_membership_from_msg(msg->view)):(NULL);

    *ma = init_membership_agreement_msg(msg->msg_type, msg->ack_status, membership, msg->nonce, vc);

    return 0;
}

int equals_membership_agreement_msg(membership_agreement_msg * ma1, membership_agreement_msg * ma2)
{
    if(ma1->nonce != ma2->nonce || ma1->msg_type != ma2->msg_type || ma1->ack_status != ma2->ack_status)
        return 0;

    if((ma1->membership != NULL && ma2->membership == NULL) ||
        (ma1->membership == NULL && ma2->membership != NULL) ||
        !equals_membership_state(ma1->membership, ma2->membership))
        return 0;

    if(compare_vc(ma1->vc, ma2->vc) != 0)
        return 0;

    return 1;
}

char * to_string_membership_agreement_msg(membership_agreement_msg * ma, char * msg_buff)
{
    char * crt_ptr = msg_buff;
    sprintf(crt_ptr, "Membership_agreement_msg(type=%d, ack_status=%d, nonce=%" PRId64 ", ", ma->msg_type, ma->ack_status, ma->nonce);
    crt_ptr += strlen(crt_ptr);

    if(ma->membership != NULL)
    {
        to_string_membership_state(ma->membership, crt_ptr);
        crt_ptr += strlen(crt_ptr);
        sprintf(crt_ptr, ", ");
        crt_ptr += 2;
    }

    if(ma->vc != NULL)
    {
        to_string_vc(ma->vc, crt_ptr);
        crt_ptr += strlen(crt_ptr);
    }
    sprintf(crt_ptr, ")");

    return msg_buff;
}



