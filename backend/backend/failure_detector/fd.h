/*
 * fd.h
 *
 * Author: aagapi
 */

#ifndef BACKEND_FAILURE_DETECTOR_FD_H_
#define BACKEND_FAILURE_DETECTOR_FD_H_

#include "backend/failure_detector/vector_clock.h"

#define MAX_MSG_SIZE_GS (MAX_MSG_SIZE_VC + 16)

#define SKIP_PROPOSAL_STATUS 10

/* Node description: */

typedef struct node_description
{
    int status;
    int node_id;
    int rack_id;
    int dc_id;

    char * hostname;
    unsigned short portno;
    struct sockaddr_in address;
} node_description;

node_description * init_node_description(int status, int node_id, int rack_id, int dc_idm, char * hostname, unsigned short portno);
int copy_node_description(node_description * nd, int status, int node_id, int rack_id, int dc_id, char * hostname, unsigned short portno);
void free_node_description(node_description * vc);
int equals_node_description(node_description * nd1, node_description * nd2);
char * to_string_node_description(node_description * nd, char * msg_buff);
void init_ns_msg_from_description(NodeStateMessage * ns_msg, node_description * nd);

/* Gossip state: */

typedef struct gossip_state
{
    node_description nd;
    vector_clock * vc;
} gossip_state;

gossip_state * init_gossip_state(int status, int node_id, int rack_id, int dc_id, char * hostname, unsigned short portno, vector_clock * vc);
void free_gossip_state(gossip_state * vc);
int serialize_gs(gossip_state * gs, void ** buf, unsigned * len);
int deserialize_gs(void * buf, unsigned msg_len, gossip_state ** gs);
int equals_gs(gossip_state * gs1, gossip_state * gs2);
char * to_string_gs(gossip_state * gs, char * msg_buff);

/* Membership view: */

typedef struct membership_state
{
    int no_nodes;
    node_description * membership;
    int no_client_nodes;
    node_description * client_membership;
    vector_clock * view_id;
} membership_state;

membership_state * init_membership_state(int no_nodes, node_description * membership, int no_client_nodes, node_description * client_membership, vector_clock * view_id);
void free_membership_state(membership_state * ms, int do_free_vc);
int serialize_membership_state(membership_state * gs, void ** buf, unsigned * len);
int deserialize_membership_state(void * buf, unsigned msg_len, membership_state ** gs);
int equals_membership_state(membership_state * gs1, membership_state * gs2);
char * to_string_membership_state(membership_state * gs, char * msg_buff);

/* Membership agreement messages: */

#define MEMBERSHIP_AGREEMENT_PROPOSE 0
#define MEMBERSHIP_AGREEMENT_RESPONSE 1
#define MEMBERSHIP_AGREEMENT_NOTIFY 2
#define MEMBERSHIP_AGREEMENT_RETRY_LINK 3
#define MEMBERSHIP_AGREEMENT_NOTIFY_ACK 4
#define MEMBERSHIP_AGREEMENT_JOIN 5

#define ACK 0
#define NACK 1
#define UNINIT 2

typedef struct membership_agreement_msg
{
    int msg_type;
    int ack_status;
    membership_state * membership;
    int64_t nonce;
    vector_clock * vc;
} membership_agreement_msg;

membership_agreement_msg * get_membership_propose_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc);
membership_agreement_msg * get_membership_response_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc);
membership_agreement_msg * get_membership_notify_msg(int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc);
membership_agreement_msg * get_membership_notify_ack_msg(int ack_status, int64_t nonce, vector_clock * vc);
membership_agreement_msg * get_membership_join_msg(int status, int rack_id, int dc_id, char * hostname, unsigned short portno, int64_t nonce, vector_clock * vc);
membership_agreement_msg * init_membership_agreement_msg(int msg_type, int ack_status, membership_state * membership, int64_t nonce, vector_clock * vc);
void free_membership_agreement(membership_agreement_msg * ma);
void free_membership_agreement_msg(MembershipAgreementMessage * msg);
int serialize_membership_agreement_msg(membership_agreement_msg * gs, void ** buf, unsigned * len);
int deserialize_membership_agreement_msg(void * buf, unsigned msg_len, membership_agreement_msg ** ma);
int equals_membership_agreement_msg(membership_agreement_msg * ma1, membership_agreement_msg * ma2);
char * to_string_membership_agreement_msg(membership_agreement_msg * gs, char * msg_buff);

#endif /* BACKEND_FAILURE_DETECTOR_FD_H_ */
