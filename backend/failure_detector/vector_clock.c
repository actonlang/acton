/*
 * vector_clock.c
 *
 *  Author: aagapi
 */

#include "vector_clock.h"
#include "db_messages.pb-c.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #include "cfuhash.h"

int increment_vc(vector_clock * vc, int node_id)
{
	// Binary search node_id:
	int found_idx = -1, exact_match = 0;
	BINARY_SEARCH_NODEID(vc, node_id, found_idx, exact_match);

	if(!exact_match)
		return -1;

	vc->node_ids[found_idx].counter++;

	return 0;
}

// Returns:
// 		-2 of vc1 and vc2 are incomparable
// 		-1 if vc1 < vc2
// 		0 if vc1 == vc2
// 		1 if vc1 > vc2

int compare_vc(vector_clock * vc1, vector_clock * vc2)
{
	if(vc1 == NULL && vc2 == NULL)
		return 0;

	if(vc1 == NULL || vc2 == NULL)
		return -2;

	if(vc1->no_nodes != vc2->no_nodes)
		return -2;

	int first_bigger = 0, second_bigger = 0;

	for(int i=0;i<vc1->no_nodes;i++)
	{
		if(vc1->node_ids[i].node_id != vc2->node_ids[i].node_id)
			return -2;

		if(vc1->node_ids[i].counter > vc2->node_ids[i].counter)
			first_bigger = 1;
		else if(vc1->node_ids[i].counter < vc2->node_ids[i].counter)
			second_bigger = 1;
	}

	if(first_bigger && second_bigger)
		return -2;
	else if(first_bigger)
		return 1;
	else if(second_bigger)
		return -1;
	else
		return 0;
}

int update_vc(vector_clock * vc_dest, vector_clock * vc_src)
{
	int dest_idx=0;

	for(int i=0;i<vc_src->no_nodes;i++)
	{
		while(vc_dest->node_ids[dest_idx].node_id < vc_src->node_ids[i].node_id && dest_idx < vc_dest->no_nodes)
			dest_idx++;

		if(vc_dest->node_ids[dest_idx].node_id > vc_src->node_ids[i].node_id)
		{
			// Source vector has a component that dest vector doesn't. Add that component to the dest vector:

			add_component_vc(vc_dest, vc_src->node_ids[i].node_id, vc_src->node_ids[i].counter);
		}
		else
		{
			// Update dest counter of this component with the maximum of the 2:
			if(vc_src->node_ids[i].counter > vc_dest->node_ids[dest_idx].counter)
				vc_dest->node_ids[dest_idx].counter = vc_src->node_ids[i].counter;
		}
	}

	return 0;
}

int update_or_replace_vc(vector_clock ** vc_dest, vector_clock * vc_src)
{
	int alloc_new_vc = (*vc_dest == NULL || (*vc_dest)->no_nodes != vc_src->no_nodes);

	if(!alloc_new_vc)
	{
		for(int n=0;n<vc_src->no_nodes;n++)
		{
			if((*vc_dest)->node_ids[n].node_id != vc_src->node_ids[n].node_id)
			{
				alloc_new_vc = 1;
				break;
			}
			else
			{
				(*vc_dest)->node_ids[n].counter = vc_src->node_ids[n].counter;
			}
		}
	}

	if(alloc_new_vc)
	{
		if(*vc_dest != NULL)
			free_vc(*vc_dest);
		(*vc_dest) = copy_vc(vc_src);
	}

	return 0;
}

int add_component_vc(vector_clock * vc, int node_id, int initial_counter)
{
	// Binary search node_id:
	int found_idx = 0, exact_match = 0;
	BINARY_SEARCH_NODEID(vc, node_id, found_idx, exact_match);

	if(exact_match)
		return -1; // Component already existed

	if(vc->no_nodes == vc->capacity)
		grow_vc(vc);

	// Insert component in its location and shift rest to keep vector sorted
	// Note that this is a rare operation:

	for(int idx = vc->no_nodes;idx>found_idx;idx--)
		vc->node_ids[idx] = vc->node_ids[idx-1];

	vc->node_ids[found_idx].node_id = node_id;
	vc->node_ids[found_idx].counter = (initial_counter > 0)?initial_counter:0;

	vc->no_nodes++;

	return 0;
}

// S'd never call this in principle:

int remove_component_vc(vector_clock * vc, int node_id)
{
	// Binary search node_id:
	int found_idx = -1, exact_match = 0;
	BINARY_SEARCH_NODEID(vc, node_id, found_idx, exact_match);

	if(!exact_match)
		return -1; // Component doesn't exist

	// Remove component and shift the rest:

	for(int idx = found_idx; idx < vc->no_nodes ;idx++)
		vc->node_ids[idx] = vc->node_ids[idx+1];

	vc->no_nodes--;

	return 0;
}

int cmpfunc (const void * a, const void * b) {
   return (((struct versioned_id *)a)->node_id - ((struct versioned_id *)b)->node_id);
}

vector_clock * init_vc(int init_no_nodes, int * node_ids, long * counters, int sort_node_ids)
{
	vector_clock * vc = (vector_clock *) malloc(sizeof(struct vector_clock));
	memset(vc, 0, sizeof(struct vector_clock));

	vc->no_nodes = (init_no_nodes > 0)? init_no_nodes:0;

	vc->capacity = (int)(((init_no_nodes > DEFAULT_SIZE)?init_no_nodes:DEFAULT_SIZE) * GROWTH_RATE);

	vc->node_ids =  (versioned_id *) malloc (vc->capacity * sizeof(struct versioned_id));
	memset(vc->node_ids, 0, vc->capacity * sizeof(struct versioned_id));

	for(int i=0;i<vc->no_nodes;i++)
	{
		vc->node_ids[i].node_id = (node_ids != NULL)? node_ids[i]:0;
		vc->node_ids[i].counter = (counters != NULL)?counters[i]:0;
	}

	if(sort_node_ids) // Only call with sort_node_ids true if input node_ids not already sorted
	{
		qsort(vc->node_ids, vc->no_nodes, sizeof(struct versioned_id), cmpfunc);
	}

	return vc;
}

vector_clock * copy_vc(vector_clock * vc1)
{
	vector_clock * vc = (vector_clock *) malloc(sizeof(struct vector_clock));

	vc->no_nodes = vc1->no_nodes;

	vc->capacity = vc1->capacity;

	vc->node_ids =  (versioned_id *) malloc (vc->capacity * sizeof(struct versioned_id));

	for(int i=0;i<vc->no_nodes;i++)
	{
		vc->node_ids[i].node_id = vc1->node_ids[i].node_id;
		vc->node_ids[i].counter = vc1->node_ids[i].counter;
	}

	return vc;
}

vector_clock * init_vc_from_msg(VectorClockMessage * msg)
{
	vector_clock * vc = init_vc(msg->n_ids, NULL, NULL, 0);

	for (int i = 0; i < vc->no_nodes; i++)
	{
		vc->node_ids[i].node_id = msg->ids[i];
		vc->node_ids[i].counter = msg->counters[i];
	}

	return vc;
}

int grow_vc(vector_clock * vc)
{
	vc->capacity = (int)(vc->no_nodes * GROWTH_RATE);

	versioned_id * new_vector = (versioned_id *) malloc (vc->capacity * sizeof (struct versioned_id));

	memcpy(new_vector, vc->node_ids, vc->no_nodes * sizeof (struct versioned_id));

	free(vc->node_ids);

	vc->node_ids = new_vector;

	return 0;
}

void free_vc(vector_clock * vc)
{
	if(vc == NULL)
		return;

	if(vc->node_ids != NULL)
		free(vc->node_ids);
	free(vc);
}

void init_vc_msg(VectorClockMessage * msg_ptr, vector_clock * vc)
{
	 msg_ptr->n_ids = vc->no_nodes;
	 msg_ptr->ids = malloc (msg_ptr->n_ids * sizeof(int));
	 msg_ptr->n_counters = vc->no_nodes;
	 msg_ptr->counters = malloc (msg_ptr->n_counters * sizeof(long));
	 for (int i = 0; i < msg_ptr->n_ids; i++)
	 {
		 msg_ptr->ids[i] = vc->node_ids[i].node_id;
		 msg_ptr->counters[i] = vc->node_ids[i].counter;
	 }
}

void free_vc_msg(VectorClockMessage * msg)
{
	free(msg->ids);
	free(msg->counters);
}

char * to_string_vc(vector_clock * vc, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	if(vc == NULL)
	{
		sprintf(crt_ptr, "VC(NULL)");
		return msg_buff;
	}

	sprintf(crt_ptr, "VC(");
	crt_ptr += strlen(crt_ptr);

	for(int i=0;i<vc->no_nodes;i++)
	{
		sprintf(crt_ptr, "%d:%ld, ", vc->node_ids->node_id, vc->node_ids->counter);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, ")");

	return msg_buff;
}



