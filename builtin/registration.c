// classid generation and retrieval ////////////////////////////////////////////////////////

/* 
 * Note that this does not attempt to be thread-safe. 
 * We need to sort out how initialization is to be done.
 */

int nextid = 100;

$dict methods;  //key is classid; value is method table
$dict classids; //key is method table; value is classid

/*
 * Probably better to let methods be a list, since it is indexed by non-negative ints.
 * Would require nextid to be initialized to 16 (first unused classid after builtin 
 * and rts classes).
 */

void $register_force(int classid, $Serializable$methods meths) {
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
    fprintf(stderr,"Internal error in get_classid: classid not found\n");
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

/*
 * We do not register rts classid's here, since we do be able to serialize without including all of rts.o with its  
 * special main and handling of $ROOT. Doing so would complicate testing of builtin types significantly.
 */
void $register_builtin() {
  methods  = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  classids = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  $register_force(NULL_ID,($Serializable$methods)&$Null$methods);
  $register_force(INT_ID,($Serializable$methods)&$int$methods);
  $register_force(FLOAT_ID,($Serializable$methods)&$float$methods);
  //  $register_force(COMPLEX_ID,($Serializable$methods)&$complex$methods);
  $register_force(BOOL_ID,($Serializable$methods)&$bool$methods);
  $register_force(STR_ID,($Serializable$methods)&$str$methods);
  $register_force(LIST_ID,($Serializable$methods)&$list$methods);
  $register_force(DICT_ID,($Serializable$methods)&$dict$methods);
  $register_force(SET_ID,($Serializable$methods)&$set$methods);
  $register_force(RANGE_ID,($Serializable$methods)&$range$methods);
  $register_force(STRITERATOR_ID,($Serializable$methods)&$Iterator$str$methods);
  $register_force(LISTITERATOR_ID,($Serializable$methods)&$Iterator$list$methods);
  $register_force(DICTITERATOR_ID,($Serializable$methods)&$Iterator$dict$methods);
  $register_force(VALUESITERATOR_ID,($Serializable$methods)&$Iterator$dict$values$methods);
  $register_force(ITEMSITERATOR_ID,($Serializable$methods)&$Iterator$dict$items$methods);
  $register_force(SETITERATOR_ID,($Serializable$methods)&$Iterator$set$methods);
  $register_force(RANGEITERATOR_ID,($Serializable$methods)&$Iterator$range$methods);
}

#define RANGE_ID 15
#define ITEM_ID 16
#define STRITERATOR_ID 17
#define LISTITERATOR_ID 18
#define DICTITERATOR_ID 19
#define VALUESITERATOR_ID 20
#define ITEMSITERATOR_ID 21
#define SETITERATOR_ID 22
#define RANGEITERATOR_ID 23
