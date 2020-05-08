#include "graph.h"
#include "../../rts/rts.h"

// Nodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $Node__init__($Node self, $list nbors);
void $Node__serialize__($Node self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum);
$Node $Node__deserialize__($Mapping$dict wit, $ROW* row, $dict done);

struct $Node$class $Node$methods = {"",NULL,$Node__init__,$Node__serialize__,$Node__deserialize__};

void $Node__init__($Node self, $list nbors) {
  self->nbors = nbors;
}

void $Node__serialize__($Node self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)&$Node$methods);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->nbors,wit,start_no,done,accum);
  }
}


$Node $Node__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $Node res = malloc(sizeof(struct $Node));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$Node$methods;
    res->nbors = ($list)$step_deserialize(wit,row,done);
    return res;
  }
}

// IntNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $IntNode__init__($IntNode self, $list nbors, $int ival);
void $IntNode__serialize__($IntNode self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum);
$IntNode $IntNode__deserialize__($Mapping$dict wit, $ROW* row, $dict done);

struct $IntNode$class $IntNode$methods = {"",NULL,$IntNode__init__,$IntNode__serialize__,$IntNode__deserialize__};

void $IntNode__init__($IntNode self, $list nbors, $int ival) {
  self->nbors = nbors;
  self->ival= ival;
}

void $IntNode__serialize__($IntNode self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)&$IntNode$methods);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->nbors,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->ival,wit,start_no,done,accum);
  }
}

$IntNode $IntNode__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $IntNode res = malloc(sizeof(struct $IntNode));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$IntNode$methods;
    res->nbors = ($list)$step_deserialize(wit,row,done);
    res->ival = ($int)$step_deserialize(wit,row,done);
    return res;
  }
}

// FloatNodes (graph vertices) ////////////////////////////////////////////////////////////////////////////

void $FloatNode__init__($FloatNode self, $list nbors, $float ival);
void $FloatNode__serialize__($FloatNode self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum);
$FloatNode $FloatNode__deserialize__($Mapping$dict wit, $ROW* row, $dict done);

struct $FloatNode$class $FloatNode$methods = {"",NULL,$FloatNode__init__,$FloatNode__serialize__,$FloatNode__deserialize__};

void $FloatNode__init__($FloatNode self, $list nbors, $float fval) {
  self->nbors = nbors;
  self->fval= fval;
}

void $FloatNode__serialize__($FloatNode self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)&$FloatNode$methods);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->nbors,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->fval,wit,start_no,done,accum);
  }
}

$FloatNode $FloatNode__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $FloatNode res = malloc(sizeof(struct $FloatNode));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$FloatNode$methods;
    res->nbors = ($list)$step_deserialize(wit,row,done);
    res->fval = ($float)$step_deserialize(wit,row,done);
   return res;
  }
}

// Graphs ////////////////////////////////////////////////////////////////////////////

void $Graph__init__($Graph self, $list nodes);
void $Graph__serialize__($Graph self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum);
$Graph $Graph__deserialize__($Mapping$dict wit, $ROW* row, $dict done);

struct $Graph$class $Graph$methods = {"",NULL,$Graph__init__,$Graph__serialize__,$Graph__deserialize__};


void $Graph__init__($Graph self, $list nodes) {
  self->nodes = nodes;
}

void $Graph__serialize__($Graph self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)&$Graph$methods);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->nodes,wit,start_no,done,accum);
  }
}


$Graph $Graph__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $Graph res = malloc(sizeof(struct $Graph));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$Graph$methods;
    res->nodes = ($list)$step_deserialize(wit,row,done);
    return res;
  }
}

void $register_graph(){
  $register(($Serializable$methods)&$Node$methods);
  $register(($Serializable$methods)&$IntNode$methods);
  $register(($Serializable$methods)&$FloatNode$methods);
  $register(($Serializable$methods)&$Graph$methods);
}
