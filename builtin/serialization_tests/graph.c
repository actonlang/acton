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

#include "graph.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $Node__init__($Node self, B_list nbors) {
  self->nbors = nbors;
}

void $Node__serialize__($Node self,$NoneType state) {
  $step_serialize(self->nbors,state); 
}

B_bool $Node__bool__($Node self) {
  return $True;
}

B_str $Node__str__($Node self) {
  return to$str("$Node");
}

$Node $Node__deserialize__($NoneType state) {                   
  $Node res = $DNEW($Node,state);                    
  res->nbors = (B_list)$step_deserialize(state);      
  return res;                                        
}

struct $NodeG_class $NodeG_methods = {"",UNASSIGNED,NULL,$Node__init__,$Node__serialize__,$Node__deserialize__,$Node__bool__,$Node__str__};

// IntNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////
 
void $IntNode__init__($IntNode self, B_list nbors, B_int ival) {
  self->nbors = nbors;
  self->ival= ival;
}

B_bool $IntNode__bool__($IntNode self) {
  return $True;
}

B_str $IntNode__str__($IntNode self) {
  return to$str("$IntNode");
}

void $IntNode__serialize__($IntNode self,$NoneType state) {
  $step_serialize(self->nbors,state);
  $step_serialize(self->ival,state);
}

$IntNode $IntNode__deserialize__($NoneType state) {
  $IntNode res = $DNEW($IntNode,state);                    
  res->nbors = (B_list)$step_deserialize(state);
  res->ival = (B_int)$step_deserialize(state);
  return res;
}

struct $IntNodeG_class $IntNodeG_methods = {"",UNASSIGNED,NULL,$IntNode__init__,$IntNode__serialize__,$IntNode__deserialize__,$IntNode__bool__,$IntNode__str__};


// FloatNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $FloatNode__init__($FloatNode self, B_list nbors, B_float fval) {
  self->nbors = nbors;
  self->fval= fval;
}

B_bool $FloatNode__bool__($FloatNode self) {
  return $True;
}

B_str $FloatNode__str__($FloatNode self) {
  return to$str("$FloatNode");
}

void $FloatNode__serialize__($FloatNode self,$NoneType state) {
  $step_serialize(self->nbors,state);
  $step_serialize(self->fval,state);
}

$FloatNode $FloatNode__deserialize__($NoneType state) {
  $FloatNode res = $DNEW($FloatNode,state);                    
  res->nbors = (B_list)$step_deserialize(state);
  res->fval = (B_float)$step_deserialize(state);
  return res;
}

struct $FloatNodeG_class $FloatNodeG_methods = {"",UNASSIGNED,NULL,$FloatNode__init__,$FloatNode__serialize__,$FloatNode__deserialize__,$FloatNode__bool__,$FloatNode__str__};


// Graphs ////////////////////////////////////////////////////////////////////////////


void $Graph__init__($Graph self, B_list nodes) {
  self->nodes = nodes;
}

B_bool $Graph__bool__($Graph self) {
  return $True;
}

B_str $Graph__str__($Graph self) {
  return to$str("$Graph");
}

void $Graph__serialize__($Graph self, $NoneType state) {
  $step_serialize(self->nodes,state);
}

$Graph $Graph__deserialize__($NoneType state) {
  $Graph res = $DNEW($Graph,state);
  res->nodes = (B_list)$step_deserialize(state);
  return res;
}

struct $GraphG_class $GraphG_methods = {"",UNASSIGNED,NULL,$Graph__init__,$Graph__serialize__,$Graph__deserialize__,$Graph__bool__,$Graph__str__};

void $register_graph(){
  $register(&$NodeG_methods);
  $register(&$IntNodeG_methods);
  $register(&$FloatNodeG_methods);
  $register(&$GraphG_methods);
}
