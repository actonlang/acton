/*
 * fd.h
 *
 * Author: aagapi
 */

#ifndef BACKEND_FAILURE_DETECTOR_FD_H_
#define BACKEND_FAILURE_DETECTOR_FD_H_

#include "vector_clock.h"

#define MAX_MSG_SIZE_GS (MAX_MSG_SIZE_VC + 16)

typedef struct gossip_state
{
	int status;
	int node_id;
	int rack_id;
	int dc_id;
	vector_clock * vc;
} gossip_state;

gossip_state * init_gossip_state(int status, int node_id, int rack_id, int dc_id, vector_clock * vc);
void free_gossip_state(gossip_state * vc);
int serialize_gs(gossip_state * gs, void ** buf, unsigned * len);
int deserialize_gs(void * buf, unsigned msg_len, gossip_state ** gs);
int equals_gs(gossip_state * gs1, gossip_state * gs2);
char * to_string_gs(gossip_state * gs, char * msg_buff);


#endif /* BACKEND_FAILURE_DETECTOR_FD_H_ */
