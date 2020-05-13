/* 
 * During initialization of an Acton program we need to establish a one-to-one mapping between method tables and integers. 
 * The latter are called class id's and for each object being serialized, its class id is stored together with serialization of its
 * local state. During deserialization, the class id is used to find the object's method table and hence its __deserialize__ method.
 * Builtin classes and classes in the runtime system have predefined class id's, mainly to ease debugging (e.g. to make reading
 * hexdumps of serialized files slighly less painful).
 * 
 * This mechanism requires that the mapping between method tables and integers is the same in the serializing and the 
 * deserializing system. This will be the case if they do registration for the same classes in the same order, typically 
 * by being the same piece of software.
 */

#define UNASSIGNED -1
#define NULL_ID 0  
#define INT_ID 1
#define FLOAT_ID 2
#define COMPLEX_ID 3
#define BOOL_ID 4
#define STR_ID 5
#define LIST_ID 6
#define DICT_ID 7
#define SET_ID 8
#define RANGE_ID 9
#define ITEM_ID 10
#define MSG_ID 11
#define ACTOR_ID 12
#define CATCHER_ID 13
#define CLOS_ID 14
#define CONT_ID 15
#define DONE_ID 16
#define RETNEW_ID 37
#define STRITERATOR_ID 17
#define LISTITERATOR_ID 18
#define DICTITERATOR_ID 19
#define VALUESITERATOR_ID 20
#define ITEMSITERATOR_ID 21
#define SETITERATOR_ID 22
#define RANGEITERATOR_ID 23

#define BASEEXCEPTION_ID                        24
#define     SYSTEMEXIT_ID                       25
#define     KEYBOARDINTERRUPT_ID                26
#define     EXCEPTION_ID                        27
#define         ASSERTIONERROR_ID               28
#define         LOOKUPERROR_ID                  29
#define             INDEXERROR_ID               30
#define             KEYERROR_ID                 31
#define         MEMORYERROR_ID                  32
#define         OSERROR_ID                      33
#define         RUNTIMEERROR_ID                 34
#define             NOTIMPLEMENTEDERROR_ID      35
#define         VALUEERROR_ID                   36

/* 
 * Register the builtin classes (those with the above class id's). This must be the first registration call,
 *  since it also initializes the data structures containing the mapping. 
 * This call does *not* register the rts class id's MSG_ID  -- ITEM_ID, which must be registered by 
 * a call to register_rts in rts.h. 
 */
void $register_builtin();

/* 
 * Register a user defined class by supplying the address to its method table. A fresh class id is generated and 
 * the internal mapping extended. Each class whose objects may be serialized must be registered.
 */
void $register($Serializable$methods meths);

/*
 * Registering a class with predetermined class id. To be used for builtin and rts classes only..
 * 
 */
void $register_force(int classid, $Serializable$methods meths);

/*
 * During serialization, get_classid is called to get the integer to be stored in the serialized file.
 */
int $get_classid($Serializable$methods meths);

/*
 * During deserialization, the method table is retrieved by calling get_methods.
 */
$Serializable$methods $get_methods(int classid);
