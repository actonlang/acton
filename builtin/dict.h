struct $dict;
typedef struct $dict *$dict;

//creates empty dictionary
$dict $dict_new(Hashable$__class__);


// $item_t is return type for iterator items.
typedef struct $item_struct {
  char *$GCINFO;
  $WORD key;
  $WORD value;
} *$item_t;


// Protocol instances ///////////////////////////////////////////////////////////////////////

Collection$__class__ Collection$dict_instance;
Iterable$__class__ Iterable$dict_instance;
Indexed$__class__ Indexed$dict_instance;
Mapping$__class__ Mapping$dict_instance;
Container_Eq$__class__ Container_Eq$dict_instance;
