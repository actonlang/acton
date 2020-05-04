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
  methods  = $new_dict(); 
  classids = $new_dict();
  $register_force(NULL_ID,($Serializable$methods)&$Null$methods);
  $register_force(INT_ID,($Serializable$methods)&$int$methods);
  $register_force(FLOAT_ID,($Serializable$methods)&$float$methods);
  //  $register_force(COMPLEX_ID,($Serializable$methods)&$complex$methods);
  $register_force(BOOL_ID,($Serializable$methods)&$bool$methods);
  $register_force(STR_ID,($Serializable$methods)&$str$methods);
  $register_force(LIST_ID,($Serializable$methods)&$list$methods);
  $register_force(DICT_ID,($Serializable$methods)&$dict$methods);
  $register_force(SET_ID,($Serializable$methods)&$set$methods);
}
