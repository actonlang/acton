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

// classid generation and retrieval ////////////////////////////////////////////////////////

/* 
 * Note that this does not attempt to be thread-safe. 
 * We need to sort out how initialization is to be done.
 */

$list $methods;  //key is classid; values are method tables


void $register_force(int classid, $WORD meths) {
  // we require that $methods is big enough to index it at classid. See register_builtin below.
  $list_setitem($methods,classid,meths);
  (($Serializable$class)meths)->$class_id = classid;
}
    
void $register($WORD meths) {
    $list_append($methods,meths);
    (($Serializable$class)meths)->$class_id = $methods->length-1;
    //printf("$register class %s at index %d\n", (($Serializable$class)meths)->$GCINFO, $methods->length-1);
}


/*
 * We do not register rts classid's here, since we want to be able to serialize without including all of rts.o with its  
 * special main and handling of $ROOT. Doing so would complicate testing of builtin types significantly.
 */
void $register_builtin() {
  $methods = $list_new(2*PREASSIGNED); //preallocate space for PREASSIGNED user classes before doubling needed
  memset($methods->data,0,PREASSIGNED*sizeof($WORD)); // initiate PREASSIGNED first slots to NULL;
  $methods->length = PREASSIGNED;
  $register_force(NONE_ID,&$NoneType$methods);
  // $register_force(ATOM_ID,&$atom$methods);
  $register_force(INT_ID,&$int$methods);
  $register_force(FLOAT_ID,&$float$methods);
  //  $register_force(COMPLEX_ID,&$complex$methods);
  $register_force(BOOL_ID,&$bool$methods);
  $register_force(STR_ID,&$str$methods);
  $register_force(LIST_ID,&$list$methods);
  $register_force(DICT_ID,&$dict$methods);
  $register_force(SET_ID,&$set$methods);
  $register_force(RANGE_ID,&$range$methods);
  $register_force(TUPLE_ID,&$tuple$methods);
  $register_force(BYTEARRAY_ID,&$bytearray$methods);
  $register_force(STRITERATOR_ID,&$Iterator$str$methods);
  $register_force(LISTITERATOR_ID,&$Iterator$list$methods);
  $register_force(DICTITERATOR_ID,&$Iterator$dict$methods);
  $register_force(VALUESITERATOR_ID,&$Iterator$dict$values$methods);
  $register_force(ITEMSITERATOR_ID,&$Iterator$dict$items$methods);
  $register_force(SETITERATOR_ID,&$Iterator$set$methods);
  $register_force(RANGEITERATOR_ID,&$Iterator$range$methods);
  $register_force(ENUMERATEITERATOR_ID,&$Iterator$enumerate$methods);
  $register_force(FILTERITERATOR_ID,&$Iterator$filter$methods);
  $register_force(MAPITERATOR_ID,&$Iterator$map$methods);
  $register_force(ZIPITERATOR_ID,&$Iterator$zip$methods);
  $register_force(BASEEXCEPTION_ID,&$BaseException$methods);
  $register_force(SYSTEMEXIT_ID,&$SystemExit$methods);
  $register_force(KEYBOARDINTERRUPT_ID,&$KeyboardInterrupt$methods);
  $register_force(EXCEPTION_ID,&$Exception$methods);
  $register_force(ASSERTIONERROR_ID,&$AssertionError$methods);
  $register_force(LOOKUPERROR_ID,&$LookupError$methods);
  $register_force(INDEXERROR_ID,&$IndexError$methods);
  $register_force(KEYERROR_ID,&$KeyError$methods);
  $register_force(MEMORYERROR_ID,&$MemoryError$methods);
  $register_force(OSERROR_ID,&$OSError$methods);
  $register_force(RUNTIMEERROR_ID,&$RuntimeError$methods);
  $register_force(NOTIMPLEMENTEDERROR_ID,&$NotImplementedError$methods);
  $register_force(VALUEERROR_ID,&$ValueError$methods);
  $register_builtin_protocols();
}


$bool issubtype(int sub_id, int ancestor_id) {
  if (sub_id == ancestor_id)
    return $True;
  $Super$class c =  ($Super$class)$GET_METHODS(sub_id)->$superclass;
  while(c)
    if(c->$class_id == ancestor_id)
      return $True;
    else
      c = c->$superclass;
  return $False;
}
