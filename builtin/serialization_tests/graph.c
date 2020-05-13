#include "graph.h"
#include "../../rts/rts.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $Node__init__($Node self, $list nbors) {
  self->nbors = nbors;
}

void $Node__serialize__($Node self,$Serial$state state) {
  $step_serialize(self->nbors,state); 
}


$Node $Node__deserialize__($Serial$state state) {                   
  $Node res = $DNEW($Node,state);                    
  res->nbors = ($list)$step_deserialize(state);      
  return res;                                        
}

struct $Node$class $Node$methods = {"",NULL,$Node__init__,$Node__serialize__,$Node__deserialize__};

// IntNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////
 
void $IntNode__init__($IntNode self, $list nbors, $int ival) {
  self->nbors = nbors;
  self->ival= ival;
}

void $IntNode__serialize__($IntNode self,$Serial$state state) {
  $step_serialize(self->nbors,state);
  $step_serialize(self->ival,state);
}

$IntNode $IntNode__deserialize__($Serial$state state) {
  $IntNode res = $DNEW($IntNode,state);                    
  res->nbors = ($list)$step_deserialize(state);
  res->ival = ($int)$step_deserialize(state);
  return res;
}

struct $IntNode$class $IntNode$methods = {"",NULL,$IntNode__init__,$IntNode__serialize__,$IntNode__deserialize__};


// FloatNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $FloatNode__init__($FloatNode self, $list nbors, $float fval) {
  self->nbors = nbors;
  self->fval= fval;
}

void $FloatNode__serialize__($FloatNode self,$Serial$state state) {
  $step_serialize(self->nbors,state);
  $step_serialize(self->fval,state);
}

$FloatNode $FloatNode__deserialize__($Serial$state state) {
  $FloatNode res = $DNEW($FloatNode,state);                    
  res->nbors = ($list)$step_deserialize(state);
  res->fval = ($float)$step_deserialize(state);
  return res;
}

struct $FloatNode$class $FloatNode$methods = {"",NULL,$FloatNode__init__,$FloatNode__serialize__,$FloatNode__deserialize__};


// Graphs ////////////////////////////////////////////////////////////////////////////


void $Graph__init__($Graph self, $list nodes) {
  self->nodes = nodes;
}

void $Graph__serialize__($Graph self, $Serial$state state) {
  $step_serialize(self->nodes,state);
}


$Graph $Graph__deserialize__($Serial$state state) {
  $Graph res = $DNEW($Graph,state);
  res->nodes = ($list)$step_deserialize(state);
  return res;
}

struct $Graph$class $Graph$methods = {"",NULL,$Graph__init__,$Graph__serialize__,$Graph__deserialize__};

void $register_graph(){
  $register(($Serializable$methods)&$Node$methods);
  $register(($Serializable$methods)&$IntNode$methods);
  $register(($Serializable$methods)&$FloatNode$methods);
  $register(($Serializable$methods)&$Graph$methods);
}
