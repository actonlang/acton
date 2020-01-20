#include <stdlib.h>
#include "protocols.h"

// protocol Eq  ////////////////////////////////////////////////////////////////////////////////////
 
Eq Eq$__pack__(Eq$__class__ __class__, $WORD __impl__) {
  Eq pack = malloc(sizeof(struct Eq));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Ord  ////////////////////////////////////////////////////////////////////////////////////
 
Ord Ord$__pack__(Ord$__class__ __class__, $WORD __impl__) {
  Ord pack = malloc(sizeof(struct Ord));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}
 
// protocol Logical  ////////////////////////////////////////////////////////////////////////////////////
 
Logical Logical$__pack__(Logical$__class__ __class__, $WORD __impl__) {
  Logical pack = malloc(sizeof(struct Logical));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Plus  ////////////////////////////////////////////////////////////////////////////////////
 
Plus Plus$__pack__(Plus$__class__ __class__, $WORD __impl__) {
  Plus pack = malloc(sizeof(struct Plus));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Minus  ////////////////////////////////////////////////////////////////////////////////////
 
Minus Minus$__pack__(Minus$__class__ __class__, $WORD __impl__) {
  Minus pack = malloc(sizeof(struct Minus));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Iterator  ////////////////////////////////////////////////////////////////////////////////////
 
Iterator Iterator$__pack__(Iterator$__class__ __class__, $WORD __impl__) {
  Iterator pack = malloc(sizeof(struct Iterator));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Eq_Hashable  ////////////////////////////////////////////////////////////////////////////////////
 
Eq_Hashable Eq_Hashable$__pack__(Eq_Hashable$__class__ __class__, $WORD __impl__) {
  Eq_Hashable pack = malloc(sizeof(struct Eq_Hashable));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Iterable  ////////////////////////////////////////////////////////////////////////////////////
 
Iterable Iterable$__pack__(Iterable$__class__ __class__, $WORD __impl__) {
  Iterable pack = malloc(sizeof(struct Iterable));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Collection  ////////////////////////////////////////////////////////////////////////////////////
 
Collection Collection$__pack__(Collection$__class__ __class__, $WORD __impl__) {
  Collection pack = malloc(sizeof(struct Collection));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Container  ///////////////////////////////////////////////////////////////////////////////////

Container_Eq Container_Eq$__pack__(Container_Eq$__class__ __class__, $WORD __impl__) {
  Container_Eq pack = malloc(sizeof(struct Container_Eq));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Indexed  ////////////////////////////////////////////////////////////////////////////////////

Indexed Indexed$__pack__(Indexed$__class__ __class__, $WORD __impl__) {
  Indexed pack = malloc(sizeof(struct Indexed));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Sliceable  ////////////////////////////////////////////////////////////////////////////////////
 
Sliceable Sliceable$__pack__(Sliceable$__class__ __class__, $WORD __impl__) {
  Sliceable pack = malloc(sizeof(struct Sliceable));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Sequence  ////////////////////////////////////////////////////////////////////////////////////
 
Sequence Sequence$__pack__(Sequence$__class__ __class__, $WORD __impl__) {
  Sequence pack = malloc(sizeof(struct Sequence));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Mapping  ////////////////////////////////////////////////////////////////////////////////////
 
Mapping Mapping$__pack__(Mapping$__class__ __class__, $WORD __impl__) {
  Mapping pack = malloc(sizeof(struct Mapping));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}

// protocol Set  ////////////////////////////////////////////////////////////////////////////////////

Set Set$__pack__(Set$__class__ __class__, $WORD __impl__) {
  Set pack = malloc(sizeof(struct Set));
  pack->$GCINFO = "pack";
  pack->__class__ = __class__;
  pack->__impl__ = __impl__;
  return pack;
}



$WORD next(Iterator it) {
  return it->__class__->__next__(it->__class__,it->__impl__);
}
