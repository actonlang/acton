
struct $dict;
typedef struct $dict *$dict;

typedef struct $entry_struct {
  char *GCINFO;
  long hash;
  $WORD key;
  $WORD value;  // deleted entry has value NULL
} *$entry_t;

typedef struct $table_struct {
  char *GCINFO;
  long tb_size;        // size of dk_indices array; must be power of 2
  long tb_usable;      // nr of unused entries in dk_entries (deleted entries are counted as used)
  long tb_nentries;    // nr of used entries in dk_entries
  int  tb_indices[];   // array of indices
                       // after this follows tb_entries array;
} *$table;

typedef struct $dict_internal_t {
  char *$GCINFO;
  long numelements;        // nr of elements in dictionary
  Eq_Hashable$__class__ h; // eq and hash function used in this dictionary
  $table table;            // the hashtable
} *$dict_internal_t;

typedef struct $item_struct {
  char *$GCINFO;
  $WORD key;
  $WORD value;
} *$item_t;

typedef void *$dict$__methods__; // All dictionary methods are from protocols

struct $dict {
  char *$GCINFO;
  $dict$__methods__ __class__;
  $dict_internal_t __internal__;
};

typedef struct dict_iterator_struct {
  char *$GCINFO;
  $dict_internal_t src;
  int nxt;
} *dict_iterator_state_t; 

//iterator_t $dict_reversed($dict dict); Only Sequence method for dictionaries.
// Should we support it? (We do so for str)

//$WORD $dict_pop($dict dict, $WORD key); pop is now a method in protocol Set
// Collection
$dict $dict_fromiter(Iterable it);
$int $dict_len($dict dict);
// Iterable
//iterator_internal_t $dict_iter($dict dict); 
// Container_Eq
$bool $dict_contains($dict dict, $WORD key);
$bool $dict_containsnot($dict dict, $WORD key);
// Indexed
$WORD $dict_getitem($dict dict, $WORD key);
void $dict_setitem($dict dict, $WORD key, $WORD value);
void $dict_delitem($dict dict,  $WORD key);
// Mapping
//iterator_internal_t $dict_keys($dict dict);
//iterator_internal_t $dict_values($dict dict);
//iterator_internal_t $dict_items($dict dict);

$WORD $dict_get($dict dict, $WORD key, $WORD deflt); // Never fails;
$WORD $dict_popitem($dict dict); // returns an item; items are returned in LIFO order
void $dict_update($dict dict, $dict other);
$WORD $dict_setdefault($dict dict, $WORD key, $WORD deflt);

$dict $dict_new(Eq_Hashable$__class__);

// Protocol instances ///////////////////////////////////////////////////////////////////////

Collection$__class__ Collection$dict_instance;
Iterable$__class__ Iterable$dict_instance;
Indexed$__class__ Indexed$dict_instance;
Mapping$__class__ Mapping$dict_instance;
Container_Eq$__class__ Container_Eq$dict_instance;

//$WORD $dict_iterator_next(dict_iterator_state_t iter);
