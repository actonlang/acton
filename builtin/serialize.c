#define BUF_SIZE 8192

// Queue implementation //////////////////////////////////////////////////////////////////

void $enqueue(struct $ROWLISTHEADER *lst, $ROW elem) {
  if (lst->last)
    lst->last->next = elem;
  else
    lst->fst = elem;
  lst->last = elem;
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

struct $Hashable$WORD$class $Hashable$WORD$methods = {"", (void (*)($Hashable$WORD))$default__init__, $Hashable$WORD_eq,$Hashable$WORD_ne,$Hashable$WORD_hash};
struct $Hashable$WORD $Hashable$WORD_instance = {&$Hashable$WORD$methods};
struct $Hashable$WORD *$Hashable$WORD$witness = &$Hashable$WORD_instance;

// Null methods for serialization of NULL ///////////////////////////////////////

void $Null__serialize__($Serializable self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  $enqueue(accum,$new_row(NULL_ID,start_no,0,NULL));
}

$Serializable $Null__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
  *row = (*row)->next;
  return NULL;
}

struct $Serializable$methods $Null$methods = {"",(void (*)($Serializable,...))$default__init__, $Null__serialize__,  $Null__deserialize__};

// small-step functions for (de)serializing the next object /////////////////////////////////////////////////


void $step_serialize($Serializable self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  if (self)
    self->$class->__serialize__(self,wit,start_no,done,accum);
  else
    $Null__serialize__(self,wit,start_no,done,accum);
}

$Serializable $step_deserialize($Mapping$dict wit,$ROW *row,$dict done) {
    return $get_methods(abs((*row)->class_id))->__deserialize__(wit,row,done);
}

void $val_serialize(int class_id, $WORD val, long *start_no, struct $ROWLISTHEADER *accum) { 
  $ROW row = $new_row(class_id,start_no,1,NULL);
  memcpy(row->blob,val,sizeof($WORD));
  $enqueue(accum,row);
}

$WORD $val_deserialize($ROW *row) {
  $WORD res;
  memcpy(&res,(*row)->blob,sizeof($WORD));
  *row = (*row)->next;
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
 
$ROW $serialize($Serializable s, long *start_no) {
  struct $ROWLISTHEADER accum = {NULL,NULL}; //malloc(sizeof(struct $ROWLISTHEADER));
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$WORD$witness);
  $dict done = $new_dict(); //wit->$class->__fromiter__(wit,NULL);
  s->$class->__serialize__(s,wit,start_no,done,&accum);
  return accum.fst;
}

void $serialize_file($Serializable s, char *file) {
  long start_no = 0;
  $write_serialized($serialize(s,&start_no),file);
}

$Serializable $deserialize($ROW row) {
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$int$witness);
  $dict done = wit->$class->__fromiter__(wit,NULL);
  return  $step_deserialize(wit,&row,done);
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
    $enqueue(&header,row);
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
