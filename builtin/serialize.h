// Clss id's //////////////////////////////////////////////////////////////////////////////////////////////

#define KEY -1
#define INT_ID 0
#define FLOAT_ID 1
#define COMPLEX_ID 2
#define BOOL_ID 3
#define STR_ID 4
#define LIST_ID 5
#define DICT_ID 6
#define SET_ID 7

// Basic unit of serialization ////////////////////////////////////////////////////////////////////////////

typedef struct $ROW *$ROW;

struct $ROW {
  int class_id;
  int key_size;
  int blob_size;
  $ROW next;
  $WORD data[];
};

typedef struct $ROWLISTHEADER *$ROWLISTHEADER;

struct $ROWLISTHEADER {
  $ROW fst;
  $ROW last;
};

// Interface to serialization /////////////////////////////////////////////////////////////////////////////

/* All serializable types should have method tables as if they were subclasses of Serializable */

typedef struct serial$__methods__ *serial$__methods__;

typedef struct Serializable {
  char *GCINFO;
  serial$__methods__ __class__;
} *Serializable;

struct serial$__methods__ {
  None (*__serialize__)(Serializable, $WORD, int, $dict, $ROWLISTHEADER); /* result returned in last, accumulating param */
  Serializable (*__deserialize__)($ROW*, $dict);
};

void serialize_file(Serializable s, long prefix[], int prefix_size, char *file);
$ROW serialize(Serializable s, long prefix[], int prefix_size);
void write_serialized($ROW row, char *file);

Serializable deserialize_file(char *file,  long *prefix, int *prefix_size);
Serializable deserialize($ROW row, long *prefix, int *prefix_size);
$ROW read_serialized(char *file);

// Internal auxiliary types /////////////////////////////////////////////////////////////////////////////

typedef struct $PREFIX  *$PREFIX;

struct $PREFIX {
  int prefix_size;
  $WORD prefix[];
};


serial$__methods__ serial$_methods[7];

// Hashable$PREFIX ////////////////////////////////////////////////////////////////////////////////////////

typedef struct Hashable$PREFIX$__class__  *Hashable$PREFIX$__class__;

typedef struct Hashable$PREFIX *Hashable$PREFIX;

struct Hashable$PREFIX$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable$PREFIX, $PREFIX, $PREFIX);
    $bool (*__ne__)(Hashable$PREFIX, $PREFIX, $PREFIX);
    $int (*__hash__)(Hashable$PREFIX, $PREFIX);
};

struct Hashable$PREFIX {
    char *GCINFO;
    Hashable$PREFIX$__class__  __class__;
};

Hashable$PREFIX Hashable$PREFIX_new();

// Hashable$WORD ////////////////////////////////////////////////////////////////////////////////////////

typedef struct Hashable$WORD$__class__  *Hashable$WORD$__class__;

typedef struct Hashable$WORD *Hashable$WORD;

struct Hashable$WORD$__class__ {
    char *GCINFO;
    $bool (*__eq__)(Hashable$WORD, $WORD, $WORD);
    $bool (*__ne__)(Hashable$WORD, $WORD, $WORD);
    $int (*__hash__)(Hashable$WORD, $WORD);
};

struct Hashable$WORD {
    char *GCINFO;
    Hashable$WORD$__class__  __class__;
};

Hashable$WORD Hashable$WORD_new();



None enqueue($ROWLISTHEADER lst, $ROW elem);

