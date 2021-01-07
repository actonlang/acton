#include "../builtin.h"
#include "graph.h"

#define SIZE 100
int main() {
  $register_builtin();
  $register_graph();
  /*
  First simple test: a graph with two nodes, with links to each other.
  
  $list nodes = $NEW($list,NULL);
  $Graph g = $NEW($Graph,nodes);

  $list nbors1 = $NEW($list,NULL);
  $int ival1 = $NEW($int,7);
  $IntNode node1 = $NEW($IntNode,nbors1,ival1);

  $list nbors2 = $NEW($list,NULL);
  $float fval2 = $NEW($float,3.14);
  $FloatNode node2 = $NEW($FloatNode,nbors2,fval2);

  $Sequence$list wit = $Sequence$list$witness;
  wit->$class->append(wit,g->nodes,node1);
  wit->$class->append(wit,g->nodes,node2);
  wit->$class->append(wit,node1->nbors,node2);
  wit->$class->append(wit,node2->nbors,node1);


  $serialize_file(($Serializable)g,"graph.bin");
  $Graph g2 = ($Graph)$deserialize_file("graph.bin");
  $serialize_file(($Serializable)g2,"graph2.bin");

  $Node n2 = wit->$class->__getitem__(wit,g2->nodes,to$int(1));
  $Node n1 = wit->$class->__getitem__(wit,n2->nbors,to$int(0));
  $int a = (($IntNode)n1)->ival;
  printf("a=%ld\n",a->val);
  */

  // We build a complete graph over SIZE nodes.
  
  $Node node[SIZE];
  $list nodes = $NEW($list,NULL,NULL);
  $Graph g = $NEW($Graph,nodes);
  for (int i=0; i<SIZE; i += 2) {
    $int ival = to$int(i);
    $float fval = to$float(i*3.1416);
    $list lst1 = $NEW($list,NULL,NULL);
    $list lst2 = $NEW($list,NULL,NULL);
    node[i] = ($Node)$NEW($IntNode,lst1,ival);
    node[i+1] = ($Node)$NEW($FloatNode,lst2,fval);
  }

  $Sequence$list wit = $Sequence$list$witness;

  for (int i=0; i<SIZE; i++)
    for (int j=0; j<SIZE; j++)
      if (i!=j)
        wit->$class->append(wit,node[i]->nbors,node[j]);

  for (int i=0; i<SIZE; i++)
    wit->$class->append(wit,g->nodes,node[i]);
  $serialize_file(($Serializable)g,"graph.bin");
  $Graph g2 = ($Graph)$deserialize_file("graph.bin");
  $serialize_file(($Serializable)g2,"graph2.bin");

  $Node n2 = wit->$class->__getitem__(wit,g2->nodes,to$int(SIZE-2)); //get node SIZE-2
  $Node n1 = wit->$class->__getitem__(wit,n2->nbors,to$int(2));      // get its nbor nr 2 (if SIZE is even and > 4, this is node[2]
  $int a = (($IntNode)n1)->ival;
  printf("a=%ld\n",a->val);                                          // should print 2
                                                                // check that graph.bin and graph2.bin are identical.
}
