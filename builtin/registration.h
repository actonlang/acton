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
#define INT_ID 1
#define FLOAT_ID 2
#define COMPLEX_ID 3
#define BOOL_ID 4
#define STR_ID 5
#define LIST_ID 6
#define DICT_ID 7
#define SET_ID 8
#define RANGE_ID 9
#define TUPLE_ID 10
#define BYTEARRAY_ID 11
#define ITEM_ID 12
#define MSG_ID 13
#define ACTOR_ID 14
#define CATCHER_ID 15
#define CLOS_ID 16
#define CONT_ID 17
#define DONE_ID 18
#define RETNEW_ID 19
#define STRITERATOR_ID 20
#define LISTITERATOR_ID 21
#define DICTITERATOR_ID 22
#define VALUESITERATOR_ID 23
#define ITEMSITERATOR_ID 24
#define SETITERATOR_ID 25
#define RANGEITERATOR_ID 26
#define ENUMERATEITERATOR_ID 27
#define FILTERITERATOR_ID 28
#define MAPITERATOR_ID 29
#define ZIPITERATOR_ID 30

#define BASEEXCEPTION_ID                        31
#define     SYSTEMEXIT_ID                       32
#define     KEYBOARDINTERRUPT_ID                33
#define     EXCEPTION_ID                        34
#define         ASSERTIONERROR_ID               35
#define         LOOKUPERROR_ID                  36
#define             INDEXERROR_ID               37
#define             KEYERROR_ID                 38
#define         MEMORYERROR_ID                  39
#define         OSERROR_ID                      40
#define         RUNTIMEERROR_ID                 41
#define             NOTIMPLEMENTEDERROR_ID      42
#define         VALUEERROR_ID                   43

#define PREASSIGNED 44


/* 
 * Register the builtin classes (those with the above class id's except MSG_ID  -- RETNEW_ID). 
 * This must be the first registration call, since it also initializes the data structures containing the mapping. 
 * This call does *not* register the rts class id's MSG_ID  -- RETNEW_ID, which must be registered by 
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

#define $GET_METHODS(classid)  (($Serializable$class)$list_getitem($methods,classid))

// list of method tables indexed by class_id. Only accessed via GET_METHODS above

$list $methods;

$bool issubtype(int sub_id, int ancestor_id); 
