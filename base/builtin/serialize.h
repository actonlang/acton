

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
    char *$GCINFO;
    B_dict done;
    $WORD (*globmap)($WORD);
    long row_no;
    $ROW row;
    $ROW fst; //not used in deserialization
};

// small-step helpers for defining serializations //////////////////////////////////////////////////

void $step_serialize($WORD self, $Serial$state state);
$WORD $step_deserialize($Serial$state state);

void $val_serialize(int class_id, $WORD val, $Serial$state state);
$WORD $val_deserialize($Serial$state state);

$ROW $add_header(int class_id, int64_t blob_size, $Serial$state state);

// Shared codecs for compiler-generated closures. Offsets and scalar sizes are
// computed by the C compiler; REF fields traverse the ordinary object graph.
#define $CLOSURE_REF 0

// Compile-time guards for field arrays shared by different typed closures.
#define $CLOSURE_FIELD_EQ(c1, f1, c2, f2) \
    (offsetof(struct c1, f1) == offsetof(struct c2, f2) && \
     sizeof(((c1)0)->f1) == sizeof(((c2)0)->f2))

struct $ClosureField {
    size_t offset;
    size_t size;
    int class_id;
};

struct $ClosureLayout {
    size_t size;
    $SerializableG_class methods;
    size_t field_count;
    const struct $ClosureField *fields;
};

void $closure_serialize($WORD self, const struct $ClosureLayout *layout, $Serial$state state);
$WORD $closure_deserialize($WORD self, const struct $ClosureLayout *layout, $Serial$state state);

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
    char *$GCINFO;
    int $class_id;
    $SuperG_class superclass;
    void (*__init__)(B_HashableD_WORD);
    void (*__serialize__)(B_HashableD_WORD,$Serial$state);
    B_HashableD_WORD (*__deserialize__)(B_HashableD_WORD,$Serial$state);
    bool (*__bool__)(B_HashableD_WORD);
    B_str (*__str__)(B_HashableD_WORD);
    B_str (*__repr__)(B_HashableD_WORD);
    bool (*__eq__)(B_HashableD_WORD, $WORD, $WORD);
    bool (*__ne__)(B_HashableD_WORD, $WORD, $WORD);
    B_NoneType (* hash)(B_HashableD_WORD, $WORD, B_hasher);
};

struct B_HashableD_WORD {
    struct B_HashableD_WORDG_class *$class;
};

extern struct B_HashableD_WORDG_class B_HashableD_WORDG_methods;
B_HashableD_WORD B_HashableD_WORDG_new();
extern struct B_HashableD_WORD *B_HashableD_WORDG_witness;
