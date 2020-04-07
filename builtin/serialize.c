// Queue implementation //////////////////////////////////////////////////////////////////

None enqueue($ROWLISTHEADER lst, $ROW elem) {
  if (lst->last)
    lst->last->next = elem;
  else
    lst->fst = elem;
  lst->last = elem;
}

/* $ROW dequeue($ROWLISTHEADER lst) { */
/*   $ROW res = lst->fst; */
/*   lst->fst = res->next; */
/*   if (lst->fst==NULL) */
/*     lst->last = NULL; */
/*   return res; */
/* } */

// Hashable$PREFIX ////////////////////////////////////////////////////////////////////////

$bool Hashable$PREFIX_eq(Hashable$PREFIX wit, $PREFIX a, $PREFIX b) {
  if (a->prefix_size != b->prefix_size)
    return $false;
  for (int i=0; i< a->prefix_size; i++)
    if (a->prefix[i] != b->prefix[i])
      return $false;
  return $true;
}

$bool Hashable$PREFIX_ne(Hashable$PREFIX wit, $PREFIX a, $PREFIX b) {
  return to$bool( !from$bool(Hashable$PREFIX_eq(wit,a,b)));
}

$int Hashable$PREFIX_hash(Hashable$PREFIX wit, $PREFIX a) {
  return to$int($PREFIX_hash(a));
}


static struct Hashable$PREFIX$__class__ Hashable$PREFIX_methods = {"",Hashable$PREFIX_eq,Hashable$PREFIX_ne,Hashable$PREFIX_hash};
static struct Hashable$PREFIX Hashable$PREFIX_instance = {"",&Hashable$PREFIX_methods};
static Hashable$PREFIX Hashable$PREFIX_witness = &Hashable$PREFIX_instance;

 
Hashable$PREFIX Hashable$PREFIX_new() {
  return Hashable$PREFIX_witness;
}

// Hashable$Word (for pointers) ////////////////////////////////////////////////////////////////////////

$bool Hashable$WORD_eq(Hashable$WORD wit, $WORD a, $WORD b) {
  return to$bool(a==b);
}

$bool Hashable$WORD_ne(Hashable$WORD wit, $WORD a, $WORD b) {
  return  to$bool(a != b);
}

$int Hashable$WORD_hash(Hashable$WORD wit, $WORD a) {
  return to$int(pointer_hash(a));
}


static struct Hashable$WORD$__class__ Hashable$WORD_methods = {"",Hashable$WORD_eq,Hashable$WORD_ne,Hashable$WORD_hash};
static struct Hashable$WORD Hashable$WORD_instance = {"",&Hashable$WORD_methods};
static Hashable$WORD Hashable$WORD_witness = &Hashable$WORD_instance;

 
Hashable$WORD Hashable$WORD_new() {
  return Hashable$WORD_witness;
}

// Serialization methods ///////////////////////////////////////////////////////////////////////////////

//serial$__methods__ serial$_methods[] =  {(serial$__methods__)$int_methods,NULL,NULL,NULL,NULL,(serial$__methods__)$list_methods};

int bytelength($ROW row) {
  int res = 0;
  while(row) {
    res += 3*sizeof(int) + (row->key_size+row->blob_size)*sizeof($WORD);
    row = row->next;
  }
  return res;
}

void write_serialized($ROW row, char *file) {
  int len = bytelength(row);
  unsigned char *buf = malloc(len);
  unsigned char *p = buf;
  while(row) {
    memcpy(p,row,3*sizeof(int));
    p += 3*sizeof(int);
    int datasize = (row->key_size+row->blob_size)*sizeof($WORD);
    memcpy(p,row->data,datasize);
    p += datasize;
    row = row->next;
  }
  FILE *fileptr = fopen(file,"wb");
  fwrite(buf,len,1,fileptr);
  fclose(fileptr);
}

$ROW serialize(Serializable s, long prefix[], int prefix_size) {
  $ROWLISTHEADER accum = malloc(sizeof(struct $ROWLISTHEADER));
  accum->fst = NULL;
  accum->last = NULL;
  $dict done = $new_dict((Hashable)Hashable$WORD_new());
  s->__class__->__serialize__(s,($WORD*)prefix,prefix_size,done,accum);
  return accum->fst;
}

void serialize_file(Serializable s, long prefix[], int prefix_size, char *file) {
  write_serialized(serialize(s,prefix,prefix_size),file);
}

Serializable deserialize($ROW row, long *prefix, int *prefix_size) {
  serial$_methods[INT_ID] = (serial$__methods__)$int_methods;
  serial$_methods[LIST_ID] = (serial$__methods__)$list_methods;
  memcpy(prefix,row->data,row->key_size*sizeof($WORD));
  *prefix_size = row->key_size;
  $dict done = $new_dict((Hashable)Hashable$PREFIX_new());
  return serial$_methods[row->class_id]->__deserialize__(&row,done);
}

$ROW read_serialized(char *file) {
  FILE *fileptr = fopen(file,"rb");
  struct stat sbuf;
  stat(file,&sbuf);
  unsigned char buf[sbuf.st_size];
  unsigned char *p = buf;
  int count = fread(buf,sizeof(buf),1,fileptr);
  struct $ROWLISTHEADER header;
  header.fst = NULL;
  header.last = NULL;
  if (count != 1) {
    fprintf(stderr,"file read error\n");
    exit(1);
  } else {
    while (p < buf + sizeof(buf)) {
      int class_id, key_size, blob_size;
      memcpy(&class_id,p,sizeof(int));
      memcpy(&key_size,p+sizeof(int),sizeof(int));
      memcpy(&blob_size,p+2*sizeof(int),sizeof(int));
      $ROW row = malloc(3*sizeof(int) + (key_size+blob_size+1) * sizeof($WORD));
      row->class_id = class_id;
      row->key_size = key_size;
      row->blob_size = blob_size;
      row->next = NULL;
      memcpy(row->data,p + 3*sizeof(int),(key_size+blob_size) * sizeof($WORD));
      enqueue(&header,row);
      p += 3*sizeof(int) + (key_size+blob_size)*sizeof($WORD);
    }
  }
  return header.fst;
}
  
Serializable deserialize_file(char *file,  long *prefix, int *prefix_size) {
  return deserialize(read_serialized(file),prefix,prefix_size);
}
