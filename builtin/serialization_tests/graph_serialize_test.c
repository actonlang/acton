/*
 * Copyright (C) 2019-2021 Data Ductus AB
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

#include "../builtin.h"
#include "graph.h"

#define SIZE 100
int main() {
  $register_builtin();
  $register_graph();
  /*
  First simple test: a graph with two nodes, with links to each other.
  
  B_list nodes = $NEW(B_list,NULL);
  $Graph g = $NEW($Graph,nodes);

  B_list nbors1 = $NEW(B_list,NULL);
  B_int ival1 = $NEW(B_int,7);
  $IntNode node1 = $NEW($IntNode,nbors1,ival1);

  B_list nbors2 = $NEW(B_list,NULL);
  B_float fval2 = $NEW(B_float,3.14);
  $FloatNode node2 = $NEW($FloatNode,nbors2,fval2);

  B_SequenceD_list wit = B_SequenceD_listG_witness;
  wit->$class->append(wit,g->nodes,node1);
  wit->$class->append(wit,g->nodes,node2);
  wit->$class->append(wit,node1->nbors,node2);
  wit->$class->append(wit,node2->nbors,node1);


  $serialize_file(($Serializable)g,"graph.bin");
  $Graph g2 = ($Graph)$deserialize_file("graph.bin");
  $serialize_file(($Serializable)g2,"graph2.bin");

  $Node n2 = wit->$class->__getitem__(wit,g2->nodes,toB_int(1));
  $Node n1 = wit->$class->__getitem__(wit,n2->nbors,toB_int(0));
  B_int a = (($IntNode)n1)->ival;
  printf("a=%ld\n",a->val);
  */

  // We build a complete graph over SIZE nodes.
  
  $Node node[SIZE];
  B_list nodes = $NEW(B_list,NULL,NULL);
  $Graph g = $NEW($Graph,nodes);
  for (int i=0; i<SIZE; i += 2) {
    B_int ival = toB_int(i);
    B_float fval = to$float(i*3.1416);
    B_list lst1 = $NEW(B_list,NULL,NULL);
    B_list lst2 = $NEW(B_list,NULL,NULL);
    node[i] = ($Node)$NEW($IntNode,lst1,ival);
    node[i+1] = ($Node)$NEW($FloatNode,lst2,fval);
  }

  B_SequenceD_list wit = B_SequenceD_listG_witness;

  for (int i=0; i<SIZE; i++)
    for (int j=0; j<SIZE; j++)
      if (i!=j)
        wit->$class->append(wit,node[i]->nbors,node[j]);

  for (int i=0; i<SIZE; i++)
    wit->$class->append(wit,g->nodes,node[i]);
  $serialize_file(($Serializable)g,"graph.bin");
  $Graph g2 = ($Graph)$deserialize_file("graph.bin");
  $serialize_file(($Serializable)g2,"graph2.bin");

  $Node n2 = wit->$class->__getitem__(wit,g2->nodes,toB_int(SIZE-2)); //get node SIZE-2
  $Node n1 = wit->$class->__getitem__(wit,n2->nbors,toB_int(2));      // get its nbor nr 2 (if SIZE is even and > 4, this is node[2]
  B_int a = (($IntNode)n1)->ival;
  printf("a=%ld\n",a->val);                                          // should print 2
                                                                // check that graph.bin and graph2.bin are identical.
}
