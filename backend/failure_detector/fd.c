/*
 * fd.c
 * Author: aagapi
 */

#include "fd.h"
#include "db_messages.pb-c.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

int serialize_gs(gossip_state * gs, void ** buf, unsigned * len)
{
	GossipMessage msg = GOSSIP_MESSAGE__INIT;

	VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;
	init_vc_msg(&vc_msg, gs->vc);

	msg.vc = &vc_msg;
	msg.node_state->status = gs->status;
	msg.node_state->node_id = gs->node_id;
	msg.node_state->rack_id = gs->rack_id;
	msg.node_state->dc_id = gs->dc_id;

	*len = gossip_message__get_packed_size (&msg);
	*buf = malloc (*len);
	gossip_message__pack (&msg, *buf);

	free_vc_msg(&vc_msg);

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
	sprintf(msg_buff, ")");

	return msg_buff;
}

