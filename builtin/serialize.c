
// Queue implementation //////////////////////////////////////////////////////////////////

void $enqueue($ROWLISTHEADER lst, $ROW elem) {
  if (lst->last)
    lst->last->next = elem;
  else
    lst->fst = elem;
  lst->last = elem;
}

/*
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
*/
 
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

void $register_force(int classid, $Serializable$methods meths) {
  // should we check whether meths is already present?
  $int classid1 =to$int(classid);
  $dict_setitem(methods,($Hashable)$Hashable$int$witness,classid1,meths);
  $dict_setitem(classids,($Hashable)$Hashable$WORD$witness,meths,classid1);
}
    
void $register($Serializable$methods meths) {
  $register_force(nextid++,meths);
}

int $get_classid($Serializable$methods meths) {
  $int classid = $dict_get(classids,($Hashable)$Hashable$WORD$witness,meths,NULL);
  if (classid)
    return (int)from$int(classid);
  else {
    fprintf(stderr,"Internalerror in get_classid: classid not found\n");
    exit(-1);
  }
}

$Serializable$methods $get_methods(int classid)  {
  $Serializable$methods meths = $dict_get(methods,($Hashable)$Hashable$int$witness,to$int((long)classid),NULL);
  if (meths)
    return meths;
  else {
    fprintf(stderr,"Internal error in get_methods: method table not found for classid %d\n",classid);
    exit(-1);
  }
}

void $register_builtin() {
  methods  = $new_dict(); 
  classids = $new_dict();
  $register_force(INT_ID,($Serializable$methods)&$int$methods);
  $register_force(FLOAT_ID,($Serializable$methods)&$float$methods);
  //  $register_force(COMPLEX_ID,($Serializable$methods)&$complex$methods);
  $register_force(BOOL_ID,($Serializable$methods)&$bool$methods);
  $register_force(STR_ID,($Serializable$methods)&$str$methods);
  $register_force(LIST_ID,($Serializable$methods)&$list$methods);
  $register_force(DICT_ID,($Serializable$methods)&$dict$methods);
  $register_force(SET_ID,($Serializable$methods)&$set$methods);
  /*
  $register_force(MSG_ID,($Serializable$methods)&$Msg$methods);
  $register_force(ACTOR_ID,($Serializable$methods)&$Actor$methods);
  $register_force(CATCHER_ID,($Serializable$methods)&$Catcher$methods);
  $register_force(CLOS_ID,($Serializable$methods)&$Clos$methods);
  $register_force(CONT_ID,($Serializable$methods)&$Cont$methods);
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
  $ROWLISTHEADER accum = malloc(sizeof(struct $ROWLISTHEADER));
  accum->fst = NULL;
  accum->last = NULL;
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$WORD$witness);
  $dict done = $new_dict(); //wit->$class->__fromiter__(wit,NULL);
  s->$class->__serialize__(s,wit,start_no,done,accum);
  return accum->fst;
}

void $serialize_file($Serializable s, char *file) {
  long start_no = 0;
  $write_serialized($serialize(s,&start_no),file);
}

$Serializable $deserialize($ROW row) {
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$int$witness);
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
  res->row_no = (*start_no)++;;
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
