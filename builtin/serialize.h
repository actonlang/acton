

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
    $dict done;
    $WORD (*globmap)
    ($WORD);
    long row_no;
    $ROW row;
    $ROW fst; //not used in deserialization
};

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

// $Hashable$WORD (for pointers) ////////////////////////////////////////////////////////////////////////

// $Hashable$WORD$witness is needed to create the Mapping$dict witness necessary for serialization.

struct $Hashable$WORD$class {
    char *$GCINFO;
    int $class_id;
    $Super$class superclass;
    void (*__init__)($Hashable$WORD);
    void (*__serialize__)($Hashable$WORD, $Serial$state);
    $Hashable$WORD (*__deserialize__)($Hashable$WORD, $Serial$state);
    $bool (*__bool__)($Hashable$WORD);
    $str (*__str__)($Hashable$WORD);
    $bool (*__eq__)($Hashable$WORD, $WORD, $WORD);
    $bool (*__ne__)($Hashable$WORD, $WORD, $WORD);
    $int (*__hash__)($Hashable$WORD, $WORD);
};

struct $Hashable$WORD {
    struct $Hashable$WORD$class *$class;
};

extern struct $Hashable$WORD$class $Hashable$WORD$methods;
$Hashable$WORD $Hashable$WORD$new();
extern struct $Hashable$WORD *$Hashable$WORD$witness;
