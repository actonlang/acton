/*
 * fd.c
 * Author: aagapi
 */

#include "fd.h"

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

void free_gossip_state(gossip_state * vc)
{

}




