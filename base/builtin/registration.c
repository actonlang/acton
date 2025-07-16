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

B_list G_methods;  //key is classid; values are method tables


void $register_force(int classid, $WORD meths) {
  // we require that G_methods is big enough to index it at classid. See register_builtin below.
  G_methods->data[classid] = meths;
  (($SerializableG_class)meths)->$class_id = classid;
}
    
void $register($WORD meths) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;    
    wit->$class->append(wit,G_methods,meths);
    (($SerializableG_class)meths)->$class_id = G_methods->length-1;
    //printf("$register class %s at index %d\n", (($SerializableG_class)meths)->$GCINFO, G_methods->length-1);
}


/*
 * We do not register rts classid's here, since we want to be able to serialize without including all of rts.o with its  
 * special main and handling of $ROOT. Doing so would complicate testing of builtin types significantly.
 */
void $register_builtin() {
  G_methods = B_listD_new(2*PREASSIGNED); //preallocate space for PREASSIGNED user classes before doubling needed
  memset(G_methods->data,0,PREASSIGNED*sizeof($WORD)); // initiate PREASSIGNED first slots to NULL;
  G_methods->length = PREASSIGNED;
  $register_force(NONE_ID,&B_NoneTypeG_methods);
  // $register_force(ATOM_ID,&B_atomG_methods);
  $register_force(INT_ID,&B_intG_methods);
  $register_force(FLOAT_ID,&B_floatG_methods);
  //  $register_force(COMPLEX_ID,&B_complexG_methods);
  $register_force(BOOL_ID,&B_boolG_methods);
  $register_force(STR_ID,&B_strG_methods);
  $register_force(LIST_ID,&B_listG_methods);
  $register_force(DICT_ID,&B_dictG_methods);
  $register_force(SET_ID,&B_setG_methods);
  $register_force(RANGE_ID,&B_rangeG_methods);
  $register_force(TUPLE_ID,&B_tupleG_methods);
  $register_force(BYTEARRAY_ID,&B_bytearrayG_methods);
  $register_force(STRITERATOR_ID,&B_IteratorB_strG_methods);
  $register_force(LISTITERATOR_ID,&B_IteratorD_listG_methods);
  $register_force(DICTITERATOR_ID,&B_IteratorD_dictG_methods);
  $register_force(VALUESITERATOR_ID,&B_IteratorD_dict_valuesG_methods);
  $register_force(ITEMSITERATOR_ID,&B_IteratorD_dict_itemsG_methods);
  $register_force(SETITERATOR_ID,&B_IteratorD_setG_methods);
  // $register_force(RANGEITERATOR_ID,&B_IteratorD_rangeG_methods);
  $register_force(ENUMERATEITERATOR_ID,&B_IteratorD_enumerateG_methods);
  // $register_force(FILTERITERATOR_ID,&B_IteratorD_filterG_methods);
  // $register_force(MAPITERATOR_ID,&B_IteratorD_mapG_methods);
  $register_force(ZIPITERATOR_ID,&B_IteratorD_zipG_methods);
  $register_force(BASEEXCEPTION_ID,&B_BaseExceptionG_methods);
  $register_force(SYSTEMEXIT_ID,&B_SystemExitG_methods);
  $register_force(KEYBOARDINTERRUPT_ID,&B_KeyboardInterruptG_methods);
  $register_force(EXCEPTION_ID,&B_ExceptionG_methods);
  $register_force(ASSERTIONERROR_ID,&B_AssertionErrorG_methods);
  $register_force(LOOKUPERROR_ID,&B_LookupErrorG_methods);
  $register_force(INDEXERROR_ID,&B_IndexErrorG_methods);
  $register_force(KEYERROR_ID,&B_KeyErrorG_methods);
  $register_force(MEMORYERROR_ID,&B_MemoryErrorG_methods);
  $register_force(OSERROR_ID,&B_OSErrorG_methods);
  $register_force(RUNTIMEERROR_ID,&B_RuntimeErrorG_methods);
  $register_force(NOTIMPLEMENTEDERROR_ID,&B_NotImplementedErrorG_methods);
  $register_force(VALUEERROR_ID,&B_ValueErrorG_methods);
  //  $register_builtin_protocols();
  $register_force(SEQ_ID,&$SEQG_methods);
  $register_force(BRK_ID,&$BRKG_methods);
  $register_force(CNT_ID,&$CNTG_methods);
  $register_force(RET_ID,&$RETG_methods);
}


B_bool issubtype(int sub_id, int ancestor_id) {
  if (sub_id == ancestor_id)
    return B_True;
  $SuperG_class c =  ($SuperG_class)$GET_METHODS(sub_id)->$superclass;
  while(c)
    if(c->$class_id == ancestor_id)
      return B_True;
    else
      c = c->$superclass;
  return B_False;
}
