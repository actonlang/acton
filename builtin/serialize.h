#include <stdarg.h>

// Fundamental class, from which all classes inherit //////////////////////////////////////////////////////

typedef struct $Initializable$methods *$Initializable$methods;

typedef struct $Initializable  *$Initializable;

struct $Initializable$methods {
  char *$GCINFO;
  //  struct $Initializable$methods *superclass;
  void (*__init__)($Initializable,...);
};

struct $Initializable {
  struct $Initializable$methods *$class;
};

// These default functions do nothing 

void $default__init__($Initializable);
void $default2__init__($Initializable, $WORD);
void $default3__init__($Initializable, $WORD,$WORD);


// Types for serialization ////////////////////////////////////////////////////////////////////////////


// The serialization of an Acton object/value is a linked list of $ROW:s.


typedef struct $ROW *$ROW;

struct $ROW {
  int class_id;
  int blob_size;
  long row_no;
  $ROW next;
  $WORD blob[];
};

struct $ROWLISTHEADER {
  $ROW fst;
  $ROW last;
};

// All serializable types must have method tables as if they were subclasses of Serializable 

typedef struct $Serializable$methods *$Serializable$methods;

typedef struct $Serializable  *$Serializable;

struct $Serializable$methods {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($Serializable,...);
  void (*__serialize__)($Serializable, $Mapping$dict, long*, $dict,struct  $ROWLISTHEADER*); /* result returned in the last, accumulating param */
  $Serializable (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $Serializable {
  struct $Serializable$methods *$class;
};


// small-step helpers for defining serializations //////////////////////////////////////////////////

void $step_serialize($Serializable self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum);
$Serializable $step_deserialize($Mapping$dict wit,$ROW *row, $dict done);

void $val_serialize(int class_id,  $WORD val, long *start_no, struct $ROWLISTHEADER *accum);
$WORD $val_deserialize($ROW *row);

// the next two functions are mainly used in serialization of builtin types

void $enqueue(struct $ROWLISTHEADER *lst, $ROW elem);
$ROW $new_row(int class_id, long *start_no, int blob_size, $WORD *blob);


// top-level functions for serialization of an object ////////////////////////////////////////////////

$ROW $serialize($Serializable s, long *start_no);
void $write_serialized($ROW row, char *file);
// $serialize_file just calls the above two functions
void $serialize_file($Serializable s, char *file);

$Serializable $deserialize($ROW row);
$ROW $read_serialized(char *file);
// $deserialize_file just calls the above two functions
$Serializable $deserialize_file(char *file);

// $Hashable$WORD (for pointers) ////////////////////////////////////////////////////////////////////////

// $Hashable$WORD$witness is needed to create the Mapping$dict witness necessary for serialization.

typedef struct $Hashable$WORD *$Hashable$WORD;

struct $Hashable$WORD$class {
    char *$GCINFO;
    $Super$class *superclass;
    void (*__init__)($Hashable$WORD);
    $bool (*__eq__)($Hashable$WORD, $WORD, $WORD);
    $bool (*__ne__)($Hashable$WORD, $WORD, $WORD);
    $int (*__hash__)($Hashable$WORD, $WORD);
};

struct $Hashable$WORD {
    struct $Hashable$WORD$class *$class;
};

struct $Hashable$WORD$class $Hashable$WORD$methods;
struct $Hashable$WORD *$Hashable$WORD$witness;
