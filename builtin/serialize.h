

// Types for serialization ////////////////////////////////////////////////////////////////////////////


// The serialization of an Acton object/value is a linked list of $ROW:s.


typedef struct $ROW *$ROW;

struct $ROW {
    $ROW next;
    int class_id;
    int blob_size;
    $WORD blob[];
};

struct $ROWLISTHEADER {
    $ROW fst;
    $ROW last;
};

//typedef struct $Serial$state *$Serial$state;

struct $Serial$state {
    GC_descr $GCdescr;
    char *$name;
    B_dict done;
    $WORD (*globmap)($WORD);
    long row_no;
    $ROW row;
    $ROW fst; //not used in deserialization
};
extern GC_word $Serial$stateD_gcbm[GC_BITMAP_SIZE(struct $Serial$state)];

// small-step helpers for defining serializations //////////////////////////////////////////////////

void $step_serialize($WORD self, $Serial$state state);
$WORD $step_deserialize($Serial$state state);

void $val_serialize(int class_id, $WORD val, $Serial$state state);
$WORD $val_deserialize($Serial$state state);

$ROW $add_header(int class_id, int blob_size, $Serial$state state);

// top-level functions for serialization of an object ////////////////////////////////////////////////

long $total_rowsize($ROW);

$ROW $serialize($Serializable s, $WORD (*globmap)($WORD));
$ROW $glob_serialize($Serializable s, $WORD (*globmap)($WORD));
void $write_serialized($ROW row, char *file);
// $serialize_file just calls the above two functions
void $serialize_file($Serializable s, char *file);

$Serializable $deserialize($ROW row, $WORD (*globmap)($WORD));
$Serializable $glob_deserialize($Serializable s, $ROW row, $WORD (*globmap)($WORD));
$ROW $read_serialized(char *file);
// $deserialize_file just calls the above two functions
$Serializable $deserialize_file(char *file);

// B_HashableD_WORD (for pointers) ////////////////////////////////////////////////////////////////////////

// B_HashableD_WORDG_witness is needed to create the MappingB_dict witness necessary for serialization.

typedef struct B_HashableD_WORD *B_HashableD_WORD;

struct B_HashableD_WORDG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class superclass;
    void (*__init__)(B_HashableD_WORD);
    void (*__serialize__)(B_HashableD_WORD,$Serial$state);
    B_HashableD_WORD (*__deserialize__)(B_HashableD_WORD,$Serial$state);
    B_bool (*__bool__)(B_HashableD_WORD);
    B_str (*__str__)(B_HashableD_WORD);
    B_str (*__repr__)(B_HashableD_WORD);
    B_bool (*__eq__)(B_HashableD_WORD, $WORD, $WORD);
    B_bool (*__ne__)(B_HashableD_WORD, $WORD, $WORD);
    B_int (*__hash__)(B_HashableD_WORD, $WORD);
};

struct B_HashableD_WORD {
    struct B_HashableD_WORDG_class *$class;
};
extern GC_word B_HashableD_WORDD_gcbm[GC_BITMAP_SIZE(struct B_HashableD_WORD)];

extern struct B_HashableD_WORDG_class B_HashableD_WORDG_methods;
B_HashableD_WORD B_HashableD_WORDG_new();
extern struct B_HashableD_WORD *B_HashableD_WORDG_witness;
