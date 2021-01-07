#include "../builtin.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

struct $Node;

typedef struct $Node *$Node;

struct $Node$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Node, $list);
    void (*__serialize__)($Node, $Serial$state);
    $Node (*__deserialize__)($Node, $Serial$state);
    $bool (*__bool__)($Node);
    $str (*__str__)($Node);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($IntNode, $list, $int);
    void (*__serialize__)($IntNode, $Serial$state);
    $IntNode (*__deserialize__)($IntNode, $Serial$state);
    $bool (*__bool__)($IntNode);
    $str (*__str__)($IntNode);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($FloatNode, $list, $float);
    void (*__serialize__)($FloatNode,$Serial$state);
    $FloatNode (*__deserialize__)($FloatNode,$Serial$state);
    $bool (*__bool__)($FloatNode);
    $str (*__str__)($FloatNode);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Graph, $list);
    void (*__serialize__)($Graph, $Serial$state);
    $Graph (*__deserialize__)($Graph, $Serial$state);
    $bool (*__bool__)($Graph);
    $str (*__str__)($Graph);
};

struct $Graph {
  struct $Graph$class *$class;
  $list nodes; 
};

extern struct $Graph$class $Graph$methods;

// register classes for serialization ////////////////////////////////////////////////////////////

void $register_graph();
