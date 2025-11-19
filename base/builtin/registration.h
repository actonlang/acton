/* 
 * During initialization of an Acton program we need to establish a one-to-one mapping between method tables and non-negative integers. 
 * The latter are called class id's and for each object being serialized, its class id is stored together with serialization of its
 * local state. During deserialization, the class id is used to find the object's method table and hence its __deserialize__ method.
 * Builtin classes and classes in the runtime system have predefined class id's, mainly to ease debugging (e.g. to make reading
 * hexdumps of serialized files slightly less painful).
 * 
 * This mechanism requires that the mapping between method tables and integers is the same in the serializing and the 
 * deserializing system. This will be the case if they do registration for the same classes in the same order, typically 
 * by being the same piece of software.
 */

#define UNASSIGNED -1
#define NONE_ID 0
#define ATOM_ID 1
#define INT_ID 2
#define FLOAT_ID 3
#define COMPLEX_ID 4
#define BOOL_ID 5
#define STR_ID 6
#define LIST_ID 7
#define DICT_ID 8
#define SET_ID 9
#define RANGE_ID 10
#define TUPLE_ID 11
#define BYTEARRAY_ID 12
#define ITEM_ID 13
#define MSG_ID 14
#define ACTOR_ID 15
#define CATCHER_ID 16
#define SLICE_ID 17   // Adding SLICE_ID by using a gap in the numbering...
#define CONT_ID 18
#define DONE_ID 19
#define CONSTCONT_ID 20
#define STRITERATOR_ID 21
#define LISTITERATOR_ID 22
#define DICTITERATOR_ID 23
#define VALUESITERATOR_ID 24
#define ITEMSITERATOR_ID 25
#define SETITERATOR_ID 26
#define RANGEITERATOR_ID 27
#define ENUMERATEITERATOR_ID 28
#define FILTERITERATOR_ID 29
#define MAPITERATOR_ID 30
#define ZIPITERATOR_ID 31

#define BASEEXCEPTION_ID                        32
#define     SYSTEMEXIT_ID                       33
#define     KEYBOARDINTERRUPT_ID                34
#define     EXCEPTION_ID                        35
#define         ASSERTIONERROR_ID               36
#define         LOOKUPERROR_ID                  37
#define             INDEXERROR_ID               38
#define             KEYERROR_ID                 39
#define         MEMORYERROR_ID                  40
#define         OSERROR_ID                      41
#define         RUNTIMEERROR_ID                 42
#define             NOTIMPLEMENTEDERROR_ID      43
#define         VALUEERROR_ID                   44

#define PROC_ID 45
#define ACTION_ID 46
#define MUT_ID 47
#define PURE_ID 48

#define SEQ_ID 49
#define BRK_ID 50
#define CNT_ID 51
#define RET_ID 52

#define I8_ID 53
#define I16_ID 54
#define I32_ID 55
#define I64_ID 56
#define U1_ID 57
#define U8_ID 58
#define U16_ID 59
#define U32_ID 60
#define U64_ID 61

#define PREASSIGNED 62


/* 
 * Register the builtin classes (those with the above class id's except MSG_ID  -- CONSTCONT_ID). 
 * This must be the first registration call, since it also initializes the data structures containing the mapping. 
 * This call does *not* register the rts class id's MSG_ID  -- CONSTCONT_ID, which must be registered by 
 * a call to register_rts in rts.h. 
 */

void $register_builtin();

/* 
 * Register a user defined class by supplying the address to its method table. A fresh class id is generated and 
 * the internal mapping extended. Each class whose objects may be serialized must be registered.
 */
void $register($WORD meths);

/*
 * Registering a class with predetermined class id. To be used for builtin and rts classes only.
 * 
 */
void $register_force(int classid, $WORD meths);

#define $GET_CLASSID(meths)  ((meths)->$class_id)

#define $GET_METHODS(classid)  (($SerializableG_class)G_methods->data[classid])

// list of method tables indexed by class_id. Only accessed via GET_METHODS above

extern B_list G_methods;

B_bool issubtype(int sub_id, int ancestor_id); 
