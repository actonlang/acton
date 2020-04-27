
// Queue implementation //////////////////////////////////////////////////////////////////

void $enqueue($ROWLISTHEADER lst, $ROW elem) {
  if (lst->last)
    lst->last->next = elem;
  else
    lst->fst = elem;
  lst->last = elem;
}

// $Hashable$PREFIX ////////////////////////////////////////////////////////////////////////

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

 
// $Hashable_Word (for pointers) ////////////////////////////////////////////////////////////////////////

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

// classid generation and retrieval ////////////////////////////////////////////////////////

int nextid = 100;

$dict methods;  //key is classid; value is method table
$dict classids; //key is method table; value is classid

void $set_classid_force(int classid, serial$methods meths) {
  // should we check whether meths is already present?
  $int classid1 =to$int(classid);
  $dict_setitem(methods,($Hashable)$Hashable$int$witness,classid1,meths);
  $dict_setitem(classids,($Hashable)$Hashable$WORD$witness,meths,classid1);
}
    
void $set_classid(serial$methods meths) {
  $set_classid_force(nextid++,meths);
}

int $get_classid(serial$methods meths) {
  $int classid = $dict_get(classids,($Hashable)$Hashable$WORD$witness,meths,NULL);
  if (classid)
    return (int)from$int(classid);
  else {
    fprintf(stderr,"Internalerror in get_classid: classid not found\n");
    exit(-1);
  }
}

serial$methods $get_methods(int classid)  {
  serial$methods meths = $dict_get(methods,($Hashable)$Hashable$int$witness,to$int(classid),NULL);
  if (meths)
    return meths;
  else {
    fprintf(stderr,"Internal error in get_methods: method table not found\n");
    exit(-1);
  }
}

void $init_serialization() {
  methods  = $new_dict(); 
  classids = $new_dict();
  $set_classid_force(INT_ID,(serial$methods)&$int$methods);
  $set_classid_force(FLOAT_ID,(serial$methods)&$float$methods);
  //  $set_classid_force(COMPLEX_ID,(serial$methods)&$complex$methods);
  $set_classid_force(BOOL_ID,(serial$methods)&$bool$methods);
  $set_classid_force(STR_ID,(serial$methods)&$str$methods);
  $set_classid_force(LIST_ID,(serial$methods)&$list$methods);
  $set_classid_force(DICT_ID,(serial$methods)&$dict$methods);
  $set_classid_force(SET_ID,(serial$methods)&$set$methods);
  /*
  $set_classid_force(MSG_ID,(serial$methods)&$Msg$methods);
  $set_classid_force(ACTOR_ID,(serial$methods)&$Actor$methods);
  $set_classid_force(CATCHER_ID,(serial$methods)&$Catcher$methods);
  $set_classid_force(CLOS_ID,(serial$methods)&$Clos$methods);
  $set_classid_force(CONT_ID,(serial$methods)&$Cont$methods);
  */
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
    
    chunk_size = 3*sizeof(int);
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
    
    chunk_size = (row->prefix_size+row->blob_size)*sizeof($WORD);
    start =  (char*)row->data;
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
 
$ROW $serialize($Serializable s, long prefix[], int prefix_size) {
  $ROWLISTHEADER accum = malloc(sizeof(struct $ROWLISTHEADER));
  accum->fst = NULL;
  accum->last = NULL;
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$WORD$witness);
  $dict done = wit->$class->__fromiter__(wit,NULL);
  s->$class->__serialize__(s,wit,($WORD*)prefix,prefix_size,done,accum);
  return accum->fst;
}

void $serialize_file($Serializable s, long prefix[], int prefix_size, char *file) {
  $write_serialized($serialize(s,prefix,prefix_size),file);
}

$Serializable $deserialize($ROW row, long *prefix, int *prefix_size) {
  memcpy(prefix,row->data,row->prefix_size*sizeof($WORD));
  *prefix_size = row->prefix_size;
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$PREFIX$witness);
  $dict done = wit->$class->__fromiter__(wit,NULL);
  return  $get_methods(row->class_id)->__deserialize__(wit,&row,done);
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
    int init[3];
    chunk_size = 3*sizeof(int);
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
    $ROW row = malloc(3*sizeof(int) + (init[1]+init[2]+1) * sizeof($WORD));
    memcpy(row,init,3*sizeof(int));
    row->next = NULL;
    chunk_size =  (init[1]+init[2]) * sizeof($WORD);
    start = (char*)row->data;
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
       
$Serializable $deserialize_file(char *file,  long *prefix, int *prefix_size) {
  return $deserialize($read_serialized(file),prefix,prefix_size);
}


$ROW $new_row(int class_id, int prefix_size, int blob_size, $WORD *prefix) {
  $ROW res = malloc(3 * sizeof(int) + (1+prefix_size+blob_size)*sizeof($WORD));
  res->class_id = class_id;
  res->prefix_size = prefix_size;
  res->blob_size = blob_size;
  res->next = 0;
  memcpy(res->data,prefix,prefix_size*sizeof($WORD));
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
