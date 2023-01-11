#include "../builtin.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

struct $Node;

typedef struct $Node *$Node;

struct $NodeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Node, B_list);
    void (*__serialize__)($Node, $NoneType);
    $Node (*__deserialize__)($Node, $NoneType);
    B_bool (*__bool__)($Node);
    B_str (*__str__)($Node);
};

struct $Node {
  struct $NodeG_class *$class;
  B_list nbors; // list of Nodes
};

extern struct $NodeG_class $NodeG_methods;

// IntNodes  ////////////////////////////////////////////////////////////////////////////

struct $IntNode;

typedef struct $IntNode *$IntNode;

struct $IntNodeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($IntNode, B_list, B_int);
    void (*__serialize__)($IntNode, $NoneType);
    $IntNode (*__deserialize__)($IntNode, $NoneType);
    B_bool (*__bool__)($IntNode);
    B_str (*__str__)($IntNode);
};

struct $IntNode {
  struct $IntNodeG_class *$class;
  B_list nbors; // list of Nodes
  B_int ival;
};

extern struct $IntNodeG_class $IntNodeG_methods;

// FloatNodes  ////////////////////////////////////////////////////////////////////////////

struct $FloatNode;

typedef struct $FloatNode *$FloatNode;

struct $FloatNodeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($FloatNode, B_list, B_float);
    void (*__serialize__)($FloatNode,$NoneType);
    $FloatNode (*__deserialize__)($FloatNode,$NoneType);
    B_bool (*__bool__)($FloatNode);
    B_str (*__str__)($FloatNode);
};

struct $FloatNode {
  struct $FloatNodeG_class *$class;
  B_list nbors; // list of Nodes
  B_float fval;
};

extern struct $FloatNodeG_class $FloatNodeG_methods;

// Graphs ////////////////////////////////////////////////////////////////////////////

typedef struct $Graph *$Graph;

struct $GraphG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Graph, B_list);
    void (*__serialize__)($Graph, $NoneType);
    $Graph (*__deserialize__)($Graph, $NoneType);
    B_bool (*__bool__)($Graph);
    B_str (*__str__)($Graph);
};

struct $Graph {
  struct $GraphG_class *$class;
  B_list nodes; 
};

extern struct $GraphG_class $GraphG_methods;

// register classes for serialization ////////////////////////////////////////////////////////////

void $register_graph();
