#define BUF_SIZE 8192

// Queue implementation //////////////////////////////////////////////////////////////////

void $enqueue($Serial$state state, $ROW elem) {
  if (state->row)
    state->row->next = elem;
  else
    state->fst = elem;
  state->row = elem;
}
void $enqueue2(struct $ROWLISTHEADER *header, $ROW elem) {
  if (header->last)
    header->last->next = elem;
  else
    header->fst = elem;
  header->last = elem;
}

/*
// Internal auxiliary types /////////////////////////////////////////////////////////////////////////////

typedef struct $PREFIX  *$PREFIX;

struct $PREFIX {
  int prefix_size;
  $WORD prefix[];
};


// $Hashable$PREFIX 

$bool $Hashable$PREFIX_eq($Hashable$PREFIX wit, $PREFIX a, $PREFIX b) {
  if (a->prefix_size != b->prefix_size)
    return $false;
  for (int i=0; i< a->prefix_size; i++)
    if (a->prefix[i] != b->prefix[i])
      return $false;
  return $true;
}

$bool $Hashable$PREFIX_ne($Hashable$PREFIX wit, $PREFIX a, $PREFIX b) {
  return to$bool( !from$bool($Hashable$PREFIX_eq(wit,a,b)));
}

$int $Hashable$PREFIX_hash($Hashable$PREFIX wit, $PREFIX a) {
  return to$int($PREFIX_hash(a));
}


struct $Hashable$PREFIX$class $Hashable$PREFIX$methods = {"", (void (*)($Hashable$PREFIX))$default__init__, $Hashable$PREFIX_eq,$Hashable$PREFIX_ne,$Hashable$PREFIX_hash};
struct $Hashable$PREFIX $Hashable$PREFIX_instance = {&$Hashable$PREFIX$methods};
struct $Hashable$PREFIX *$Hashable$PREFIX$witness = &$Hashable$PREFIX_instance;
*/



$bool $Hashable$WORD_eq($Hashable$WORD wit, $WORD a, $WORD b) {
  return to$bool(a==b);
}

$bool $Hashable$WORD_ne($Hashable$WORD wit, $WORD a, $WORD b) {
  return  to$bool(a != b);
}

$int $Hashable$WORD_hash($Hashable$WORD wit, $WORD a) {
  return to$int(pointer_hash(a));
}

struct $Hashable$WORD$class $Hashable$WORD$methods = {"",NULL,(void (*)($Hashable$WORD))$default__init__, $Hashable$WORD_eq,$Hashable$WORD_ne,$Hashable$WORD_hash};
struct $Hashable$WORD $Hashable$WORD_instance = {&$Hashable$WORD$methods};
struct $Hashable$WORD *$Hashable$WORD$witness = &$Hashable$WORD_instance;

// Null methods for serialization of NULL ///////////////////////////////////////

void $Null__serialize__($Serializable self, $Serial$state state) {
  $enqueue(state,$new_row(NULL_ID,&state->row_no,0,NULL));
}

$Serializable $Null__deserialize__( $Serial$state state) {
  state->row = state->row->next;
  state->row_no++;
  return NULL;
}

struct $Serializable$methods $Null$methods = {"",NULL,(void (*)($Serializable,...))$default__init__, $Null__serialize__,  $Null__deserialize__};

// small-step functions for (de)serializing the next object /////////////////////////////////////////////////

void $step_serialize($WORD self, $Serial$state state) {
  if (self) {
    int class_id = $get_classid((($Serializable)self)->$class);
    if (class_id > 10) { // not one of the Acton builtin datatypes, which have hand-crafted serializations
      $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
      if (prevkey) {
        $val_serialize(-class_id,&prevkey->val,state);
      } else {
        $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
        $enqueue(state,$new_row(class_id,&state->row_no,0,NULL));
        (($Serializable)self)->$class->__serialize__(self,state);
      }
    } else 
       (($Serializable)self)->$class->__serialize__(self,state);
  } else
      $enqueue(state,$new_row(NULL_ID,&state->row_no,0,NULL));
}

$Serializable $step_deserialize($Serial$state state) {
  if (abs(state->row->class_id) > 10) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) 
      return ($Serializable)$dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
    else 
      return $get_methods(this->class_id)->__deserialize__(state);
  } else
    return $get_methods(abs(state->row->class_id))->__deserialize__(state);
}



void $val_serialize(int class_id, $WORD val,$Serial$state state) { 
  $ROW row = $new_row(class_id,&state->row_no,1,NULL);
  memcpy(row->blob,val,sizeof($WORD));
  $enqueue(state,row);
}

$WORD $val_deserialize($Serial$state state) {
  $WORD res;
  memcpy(&res,(state->row)->blob,sizeof($WORD));
  state->row = state->row->next;
  state->row_no++;
  return res;
}


// Serialization methods ///////////////////////////////////////////////////////////////////////////////

void $write_serialized($ROW row, char *file) {
  char buf[BUF_SIZE];
  char *p = buf;
  char *bufend = buf + BUF_SIZE;
  FILE *fileptr = fopen(file,"wb");
  int chunk_size;
  char *start;
  while(row) {
    chunk_size = 2*sizeof(int) + sizeof($WORD);
    start = (char*)row;
    if (p+chunk_size > bufend) {
      int fits = bufend - p;
      memcpy(p,start,fits);
      fwrite(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
      p = buf;
      chunk_size -= fits;
      start += fits;
    }
    memcpy(p,start,chunk_size);
    p+=chunk_size;
    
    chunk_size = (row->blob_size)*sizeof($WORD);
    start =  (char*)row->blob;
    while (p+chunk_size > bufend) {
      int fits = bufend - p;
      memcpy(p,start,fits);
      fwrite(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
      p = buf;
      chunk_size -= fits;
      start += fits;
    }
    memcpy(p,start,chunk_size);
    p+=chunk_size;
    
    row = row->next;
  }
  fwrite(buf,1,p-buf,fileptr); // TODO:  handle file write error
  fclose(fileptr);
}
 
$ROW $serialize($Serializable s) {
  $Serial$state state = malloc(sizeof(struct $Serial$state));
  state-> done = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  state->row_no=0;
  state->row = NULL;
  state->fst = NULL;
  $step_serialize(s,state);
  return state->fst;
}

void $serialize_file($Serializable s, char *file) {
  $write_serialized($serialize(s),file);
}

$Serializable $deserialize($ROW row) {
  $Serial$state state = malloc(sizeof(struct $Serial$state));
  state-> done = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  state->row_no=0;
  state->row = row;
  state->fst = NULL;
  $dict done = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  return $step_deserialize(state);
}

$ROW $read_serialized(char *file) {
  char buf[BUF_SIZE];
  char *p = buf;
  char *bufend;
  FILE *fileptr = fopen(file,"rb");
  int chunk_size;
  char *start;
  struct $ROWLISTHEADER header;
  header.fst = NULL;
  header.last = NULL;
  bufend = buf + fread(buf,1,sizeof(buf),fileptr);
  while(p < bufend || !feof(fileptr)) {
    int init[4];
    chunk_size = 4*sizeof(int); //really two int's and a $WORD!!
    start = (char*)init;
    if (p + chunk_size > bufend) {
      int fits = bufend - p;
      memcpy(start,p,fits);
      bufend = buf + fread(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
      p = buf;
      chunk_size -= fits;
      start += fits;
    }
    memcpy(start,p,chunk_size);
    p+=chunk_size;
    $ROW row = malloc(2*sizeof(int) + (init[1]+2) * sizeof($WORD));
    memcpy(row,init,4*sizeof(int));
    row->next = NULL;
    chunk_size =  (init[1]) * sizeof($WORD);
    start = (char*)row->blob;
    while (p + chunk_size > bufend) {
      int fits = bufend - p;
      memcpy(start,p,fits);
      bufend = buf + fread(buf,1,sizeof(buf),fileptr); // TODO:  handle file write error
      p = buf;
      chunk_size -= fits;
      start += fits;
    }
    memcpy(start,p,chunk_size);
    p+=chunk_size;
    $enqueue2(&header,row);
  }
  return header.fst;
}
       
$Serializable $deserialize_file(char *file) {
  return $deserialize($read_serialized(file));
}


$ROW $new_row(int class_id, long *start_no, int blob_size, $WORD *blob) {
  $ROW res = malloc(2 * sizeof(int) + (2+blob_size)*sizeof($WORD));
  res->class_id = class_id;
  res->row_no = (*start_no)++;
  res->blob_size = blob_size;
  res->next = NULL;
  if (blob)
    memcpy(res->blob,blob,blob_size*sizeof($WORD));
  return res;
}

void $default__init__($Initializable s) {
  return;
}
void $default2__init__($Initializable s, $WORD w) {
  return;
}
void $default3__init__($Initializable s, $WORD w, $WORD w2) {
  return;
}
