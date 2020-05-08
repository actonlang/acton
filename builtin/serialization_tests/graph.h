#include "../builtin.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

struct $Node;

typedef struct $Node *$Node;

struct $Node$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Node, $list);
    void (*__serialize__)($Node, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
    $Node (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $Node {
  struct $Node$class *$class;
  $list nbors; // list of Nodes
};

extern struct $Node$class $Node$methods;

// IntNodes  ////////////////////////////////////////////////////////////////////////////

struct $IntNode;

typedef struct $IntNode *$IntNode;

struct $IntNode$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($IntNode, $list, $int);
    void (*__serialize__)($IntNode, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
    $IntNode (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $IntNode {
  struct $IntNode$class *$class;
  $list nbors; // list of Nodes
  $int ival;
};

extern struct $IntNode$class $IntNode$methods;

// FloatNodes  ////////////////////////////////////////////////////////////////////////////

struct $FloatNode;

typedef struct $FloatNode *$FloatNode;

struct $FloatNode$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($FloatNode, $list, $float);
    void (*__serialize__)($FloatNode, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
    $FloatNode (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $FloatNode {
  struct $FloatNode$class *$class;
  $list nbors; // list of Nodes
  $float fval;
};

extern struct $FloatNode$class $FloatNode$methods;

// Graphs ////////////////////////////////////////////////////////////////////////////

typedef struct $Graph *$Graph;

struct $Graph$class {
    char *$GCINFO;
    $Super$class $superclass;
    void (*__init__)($Graph, $list);
    void (*__serialize__)($Graph, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
    $Graph (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $Graph {
  struct $Graph$class *$class;
  $list nodes; 
};

extern struct $Graph$class $Graph$methods;

// register classes for serialization ////////////////////////////////////////////////////////////

void $register_graph();
