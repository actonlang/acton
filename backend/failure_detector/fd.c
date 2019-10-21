/*
 * fd.c
 * Author: aagapi
 */

#include "fd.h"
#include "db_messages.pb-c.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Gossip state: */

gossip_state * init_gossip_state(int status, int node_id, int rack_id, int dc_id, vector_clock * vc)
{
	gossip_state * gs = (gossip_state *) malloc(sizeof(struct gossip_state));

	gs->status = status;
	gs->node_id = node_id;
	gs->rack_id = rack_id;
	gs->dc_id = dc_id;
	gs->vc = (vc != NULL)?vc:init_vc(0, NULL, NULL, 0);

	return gs;
}

void free_gossip_state(gossip_state * gs)
{
	free_vc(gs->vc);
	free(gs);
}

void init_ns_msg(NodeStateMessage * ns_msg, gossip_state * gs)
{
	ns_msg->status = gs->status;
	ns_msg->node_id = gs->node_id;
	ns_msg->rack_id = gs->rack_id;
	ns_msg->dc_id = gs->dc_id;
}

void init_ns_msg_from_description(NodeStateMessage * ns_msg, node_description * nd)
{
	ns_msg->status = nd->status;
	ns_msg->node_id = nd->node_id;
	ns_msg->rack_id = nd->rack_id;
	ns_msg->dc_id = nd->dc_id;
}

void free_gossip_msg(GossipMessage * msg)
{
	free_vc_msg(msg->vc);
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
	    fprintf(stderr, "error unpacking gossip_state message\n");
	    return 1;
	  }

	  vector_clock * vc = init_vc_from_msg(msg->vc);

	  *gs = init_gossip_state(msg->node_state->status, msg->node_state->node_id, msg->node_state->rack_id, msg->node_state->dc_id, vc);

	  return 0;
}

int equals_gs(gossip_state * gs1, gossip_state * gs2)
{
	return gs1->status == gs2->status &&
			gs1->node_id == gs2->node_id &&
			gs1->rack_id == gs2->rack_id &&
			gs1->dc_id == gs2->dc_id &&
			compare_vc(gs1->vc, gs2->vc) == 0;
}

char * to_string_gs(gossip_state * gs, char * msg_buff)
{
	sprintf(msg_buff, "GS(status=%d, node_id=%d, rack_id=%d, dc_id=%d, vc=", gs->status, gs->node_id, gs->rack_id, gs->dc_id);
	to_string_vc(gs->vc, msg_buff + strlen(msg_buff));
	sprintf(msg_buff + strlen(msg_buff), ")");

	return msg_buff;
}

/* Node description: */

node_description * alloc_node_description()
{
	return (node_description *) malloc(sizeof(node_description));
}

void init_node_description(node_description * nd, int status, int node_id, int rack_id, int dc_id)
{
	nd->status = status;
	nd->node_id = node_id;
	nd->rack_id = rack_id;
	nd->dc_id = dc_id;
}

void free_node_description(node_description * vc)
{
	free(vc);
}

int equals_node_description(node_description * nd1, node_description * nd2)
{
	return nd1->node_id == nd2->node_id && nd1->rack_id == nd2->rack_id && nd1->dc_id == nd2->dc_id;
}

char * to_string_node_description(node_description * nd, char * msg_buff)
{
	sprintf(msg_buff, "Node(status=%d, node_id=%d, rack_id=%d, dc_id=%d)", nd->status, nd->node_id, nd->rack_id, nd->dc_id);
	return msg_buff;
}

/* Membership: */

membership_state * init_membership(int no_nodes, node_description * membership, vector_clock * view_id)
{
	membership_state * ms = (membership_state *) malloc(sizeof(membership_state));
	ms->no_nodes = no_nodes;
	ms->membership = membership;
	ms->view_id = view_id;
	return ms;
}

void free_membership(membership_state * ms)
{
	free(ms->membership);
	free_vc(ms->view_id);
	free(ms);
}

void free_membership_msg(MembershipViewMessage * msg)
{
	free_vc_msg(msg->view_id);

	for(int i=0;i<msg->n_membership;i++)
		free(msg->membership[i]);

	free(msg->membership);
}

void init_membership_msg(MembershipViewMessage * msg, membership_state * m)
{
	NodeStateMessage **membership_v = (NodeStateMessage **) malloc (m->no_nodes * sizeof (NodeStateMessage*));
	for(int i = 0; i < m->no_nodes; i++)
	{
		membership_v[i] = malloc (sizeof (NodeStateMessage));
	    node_state_message__init(membership_v[i]);
	    init_ns_msg_from_description(membership_v[i], m->membership+i);
	}

	msg->n_membership = m->no_nodes;
	msg->membership = membership_v;
}

int serialize_membership(membership_state * m, void ** buf, unsigned * len)
{
	MembershipViewMessage msg = MEMBERSHIP_VIEW_MESSAGE__INIT;
	VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;
	init_vc_msg(&vc_msg, m->view_id);
	msg.view_id = &vc_msg;

	init_membership_msg(&msg, m);

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
		init_node_description(membership+i, msg->membership[i]->status, msg->membership[i]->node_id, msg->membership[i]->rack_id, msg->membership[i]->dc_id);
	return init_membership(msg->n_membership, membership, view_id);
}

int deserialize_membership(void * buf, unsigned msg_len, membership_state ** ms)
{
	MembershipViewMessage * msg = membership_view_message__unpack(NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking membership view message\n");
	    return 1;
	}

	*ms = init_membership_from_msg(msg);

	return 0;
}

int equals_membership(membership_state * gs1, membership_state * gs2)
{
	if(gs1->no_nodes != gs2->no_nodes)
		return 0;

	for(int i=0;i<gs1->no_nodes;i++)
		if(!equals_node_description(gs1->membership + i, gs2->membership + i))
			return 1;

	return 0;
}

char * to_string_membership(membership_state * gs, char * msg_buff)
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

	return msg_buff;
}


/* Membership agreement messages: */

membership_agreement_msg * init_membership_agreement_msg(int msg_type, int ack_status, membership_state * membership, vector_clock * vc)
{
	membership_agreement_msg * ma = (membership_agreement_msg *) malloc(sizeof(membership_agreement_msg));
	ma->msg_type = msg_type;
	ma->ack_status = ack_status;
	ma->membership = membership;
	ma->vc = vc;
	return ma;
}

void free_membership_agreement(membership_agreement_msg * ma)
{
	free_membership(ma->membership);
	free_vc(ma->vc);
	free(ma);
}

void free_membership_agreement_msg(MembershipAgreementMessage * msg)
{
	free_vc_msg(msg->vc);
	free_membership_msg(msg->view);
}

int serialize_membership_agreement_msg(membership_agreement_msg * ma, void ** buf, unsigned * len)
{
	MembershipAgreementMessage msg = MEMBERSHIP_AGREEMENT_MESSAGE__INIT;

	VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;
	init_vc_msg(&vc_msg, ma->vc);
	msg.vc = &vc_msg;

	MembershipViewMessage mview_msg = MEMBERSHIP_VIEW_MESSAGE__INIT;
	init_membership_msg(&mview_msg, ma->membership);
	msg.view = &mview_msg;

	msg.msg_type = ma->msg_type;
	msg.ack_status = ma->ack_status;

	*len = membership_agreement_message__get_packed_size (&msg);
	*buf = malloc (*len);
	membership_agreement_message__pack (&msg, *buf);

	free_membership_agreement_msg(&msg);

	return 0;
}

int deserialize_membership_agreement_msg(void * buf, unsigned msg_len, membership_agreement_msg ** ma)
{
	MembershipAgreementMessage * msg = membership_agreement_message__unpack(NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking membership agreement message\n");
	    return 1;
	}

	vector_clock * vc = init_vc_from_msg(msg->vc);
	membership_state * membership = init_membership_from_msg(msg->view);

	*ma = init_membership_agreement_msg(msg->msg_type, msg->ack_status, membership, vc);

	return 0;
}

char * to_string_membership_agreement_msg(membership_agreement_msg * ma, char * msg_buff)
{
	char * crt_ptr = msg_buff;
	sprintf(crt_ptr, "Membership_agreement_msg(type=%d, ack_status=%d, ", ma->msg_type, ma->ack_status);
	crt_ptr += strlen(crt_ptr);

	to_string_membership(ma->membership, crt_ptr);
	crt_ptr += strlen(crt_ptr);
	sprintf(crt_ptr, ", ");
	crt_ptr += 2;

	to_string_vc(ma->vc, crt_ptr);
	crt_ptr += strlen(crt_ptr);
	sprintf(crt_ptr, ")");

	return msg_buff;
}


