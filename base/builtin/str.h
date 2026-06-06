struct B_strG_class;

struct B_str {
    struct B_strG_class *$class;
    int nbytes;              // length of str in bytes
    int nchars;              // length of str in Unicode chars
    unsigned char *str;      // str is UTF-8 encoded.
};

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a B_str value.
B_str to$str(char *str);  //Dare not remove this

B_str toB_str(char *str);

B_str to_str_noc(char *str);

// Destructor; recover the internal string.
unsigned char *fromB_str(B_str str);

B_str $FORMAT(const char *format, ...);

// Iterators over str's ///////////////////////////////////////////////////////

typedef struct B_IteratorB_str *B_IteratorB_str; ;

struct B_IteratorB_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorB_str, B_str);
    void (*__serialize__)(B_IteratorB_str,$Serial$state);
    B_IteratorB_str (*__deserialize__)(B_IteratorB_str,$Serial$state);
    B_bool (*__bool__)(B_IteratorB_str);
    B_str (*__str__)(B_IteratorB_str);
    B_str (*__repr__)(B_IteratorB_str);
    B_str (*__next__)(B_IteratorB_str);
};

struct B_IteratorB_str {
    struct B_IteratorB_strG_class *$class;
    B_str src;
    int nxt;
};

extern struct  B_IteratorB_strG_class  B_IteratorB_strG_methods;
B_IteratorB_str B_IteratorB_strG_new(B_str);

// bytearray /////////////////////////////////////////////////////////////////////////////////////



struct B_bytearray {
    struct B_bytearrayG_class *$class;
    int nbytes;
    unsigned char *str;
    int capacity;
};

 
B_bytearray toB_bytearray(char *str); 
unsigned char *fromB_bytearray(B_bytearray b);

// Iterators over bytearrays ///////////////////////////////////////////////////////

typedef struct B_IteratorB_bytearray *B_IteratorB_bytearray; ;

struct B_IteratorB_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorB_bytearray, B_bytearray);
    void (*__serialize__)(B_IteratorB_bytearray,$Serial$state);
    B_IteratorB_bytearray (*__deserialize__)(B_IteratorB_bytearray,$Serial$state);
    B_bool (*__bool__)(B_IteratorB_bytearray);
    B_str (*__str__)(B_IteratorB_bytearray);
    B_str (*__repr__)(B_IteratorB_bytearray);
    B_int (*__next__)(B_IteratorB_bytearray);
};

struct B_IteratorB_bytearray {
    struct B_IteratorB_bytearrayG_class *$class;
    B_bytearray src;
    int nxt;
};

extern struct  B_IteratorB_bytearrayG_class  B_IteratorB_bytearrayG_methods;
B_IteratorB_bytearray B_IteratorB_bytearrayG_new(B_bytearray);

// bytes /////////////////////////////////////////////////////////////////////////////////////


struct B_bytes {
    struct B_bytesG_class *$class;
    int nbytes;
    unsigned char *str;
};

B_bytes to$bytes(char *str);
B_bytes to$bytesD_len(char *str, int len);
B_bytes actBytesFromCString(char *str);
B_bytes actBytesFromCStringNoCopy(char *str);
B_bytes actBytesFromCStringLength(char *str, int len);
B_bytes actBytesFromCStringLengthNoCopy(char *str, int length);
char *fromB_bytes(B_bytes b);


// Iterators over bytes ///////////////////////////////////////////////////////


typedef struct B_IteratorB_bytes *B_IteratorB_bytes; ;

struct B_IteratorB_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorB_bytes, B_bytes);
    void (*__serialize__)(B_IteratorB_bytes,$Serial$state);
    B_IteratorB_bytes (*__deserialize__)(B_IteratorB_bytes,$Serial$state);
    B_bool (*__bool__)(B_IteratorB_bytes);
    B_str (*__str__)(B_IteratorB_bytes);
    B_str (*__repr__)(B_IteratorB_bytes);
    B_int (*__next__)(B_IteratorB_bytes);
};

struct B_IteratorB_bytes {
    struct B_IteratorB_bytesG_class *$class;
    B_bytes src;
    int nxt;
};

extern struct  B_IteratorB_bytesG_class  B_IteratorB_bytesG_methods;
B_IteratorB_bytes B_IteratorB_bytesG_new(B_bytes);

// Internal auxiliary function /////////////////////////////////////////////

// used in defining __str__ method for collection types (list, dict, set)
B_str B_strD_join_par(char lpar,B_list elems, char rpar);

B_str $default__str__(B_value);

B_bool B_OrdD_strD___eq__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_OrdD_strD___ne__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_OrdD_strD___lt__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_OrdD_strD___le__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_OrdD_strD___ge__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_OrdD_strD___gt__ (B_OrdD_str wit, B_str a, B_str b);
B_bool B_HashableD_strD___eq__ (B_HashableD_str wit, B_str a, B_str b);
B_bool B_HashableD_strD___ne__ (B_HashableD_str wit, B_str a, B_str b);
B_NoneType B_HashableD_strD_hash(B_HashableD_str wit, B_str a, B_hasher h);
B_str B_TimesD_strD___add__ (B_TimesD_str wit, B_str s, B_str t);
B_str B_TimesD_strD___iadd__ (B_TimesD_str wit, B_str s, B_str t);
B_str B_TimesD_strD___zero__ (B_TimesD_str wit);
B_str B_TimesD_strD___mul__ (B_TimesD_str wit, B_str a, B_int n);
B_str B_TimesD_strD___imul__ (B_TimesD_str wit, B_str a, B_int n);
B_str B_ContainerD_strD___fromiter__ (B_ContainerD_str wit, B_Iterable wit2, $WORD iter);
int64_t B_ContainerD_strD___len__ (B_ContainerD_str wit, B_str s);
B_bool B_ContainerD_strD___contains__ (B_ContainerD_str wit, B_str s, B_str sub);
B_bool B_ContainerD_strD___containsnot__ (B_ContainerD_str wit, B_str s, B_str sub);
B_Iterator B_ContainerD_strD___iter__ (B_ContainerD_str wit, B_str s);
B_str B_SliceableD_strD___getitem__ (B_SliceableD_str wit, B_str s, B_int i);
B_NoneType B_SliceableD_strD___setitem__ (B_SliceableD_str wit, B_str str, B_int i, B_str val);
B_NoneType B_SliceableD_strD___delitem__ (B_SliceableD_str wit, B_str str, B_int i);
B_str B_SliceableD_strD___getslice__ (B_SliceableD_str wit, B_str s, B_slice slc);
B_NoneType B_SliceableD_strD___setslice__ (B_SliceableD_str wit, B_str str, B_Iterable wit2, B_slice slc, $WORD iter);
B_NoneType B_SliceableD_strD___delslice__ (B_SliceableD_str wit, B_str str, B_slice slc);

B_bool B_OrdD_bytearrayD___eq__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
B_bool B_OrdD_bytearrayD___ne__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
B_bool B_OrdD_bytearrayD___lt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
B_bool B_OrdD_bytearrayD___le__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
B_bool B_OrdD_bytearrayD___ge__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
B_bool B_OrdD_bytearrayD___gt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b);
// B_bool B_HashableD_bytearrayD___eq__ (B_HashableD_bytearray wit, B_bytearray a, B_bytearray b);
// B_bool B_HashableD_bytearrayD___ne__ (B_HashableD_bytearray wit, B_bytearray a, B_bytearray b);
// B_NoneType B_HashableD_bytearrayD_hash(B_HashableD_bytearray wit, B_bytearray a, B_hasher h);
B_bytearray B_TimesD_SequenceD_bytearrayD___add__ (B_TimesD_SequenceD_bytearray wit, B_bytearray s, B_bytearray t);
B_bytearray B_TimesD_SequenceD_bytearrayD___iadd__ (B_TimesD_SequenceD_bytearray wit, B_bytearray s, B_bytearray t);
B_bytearray B_TimesD_SequenceD_bytearrayD___zero__ (B_TimesD_SequenceD_bytearray wit);
B_bytearray B_TimesD_SequenceD_bytearrayD___mul__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_int n);
B_bytearray B_TimesD_SequenceD_bytearrayD___imul__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_int n);
B_bytearray B_ContainerD_bytearrayD___fromiter__ (B_ContainerD_bytearray wit, B_Iterable wit2, $WORD iter);
int64_t B_ContainerD_bytearrayD___len__ (B_ContainerD_bytearray wit, B_bytearray s);
B_bool B_ContainerD_bytearrayD___contains__ (B_ContainerD_bytearray wit, B_bytearray s, B_int sub);
B_bool B_ContainerD_bytearrayD___containsnot__ (B_ContainerD_bytearray wit, B_bytearray s, B_int sub);
B_Iterator B_ContainerD_bytearrayD___iter__ (B_ContainerD_bytearray wit, B_bytearray s);
B_int B_SequenceD_bytearrayD___getitem__ (B_SequenceD_bytearray wit, B_bytearray s, B_int i);
B_NoneType B_SequenceD_bytearrayD___setitem__ (B_SequenceD_bytearray wit, B_bytearray s, B_int i, B_int val);
B_NoneType B_SequenceD_bytearrayD___delitem__ (B_SequenceD_bytearray wit, B_bytearray s, B_int i);
B_NoneType B_SequenceD_bytearrayD_insert(B_SequenceD_bytearray wit, B_bytearray self, int64_t n, B_int elem);
B_NoneType B_SequenceD_bytearrayD_append(B_SequenceD_bytearray wit, B_bytearray self, B_int elem);
B_NoneType B_SequenceD_bytearrayD_reverse(B_SequenceD_bytearray wit, B_bytearray self);
B_Iterator B_SequenceD_bytearrayD___reversed__(B_SequenceD_bytearray wit, B_bytearray self);
B_bytearray B_SequenceD_bytearrayD___getslice__ (B_SequenceD_bytearray wit, B_bytearray self, B_slice slc);
B_NoneType B_SequenceD_bytearrayD___setslice__ (B_SequenceD_bytearray wit,  B_bytearray self, B_Iterable wit2, B_slice slc, $WORD iter);
B_NoneType B_SequenceD_bytearrayD___delslice__ (B_SequenceD_bytearray wit,  B_bytearray self, B_slice slc);

B_bool B_OrdD_bytesD___eq__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bool B_OrdD_bytesD___ne__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bool B_OrdD_bytesD___lt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bool B_OrdD_bytesD___le__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bool B_OrdD_bytesD___ge__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bool B_OrdD_bytesD___gt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b);
B_bytes B_ContainerD_bytesD___fromiter__ (B_ContainerD_bytes wit, B_Iterable wit2, $WORD iter);
int64_t B_ContainerD_bytesD___len__ (B_ContainerD_bytes wit, B_bytes s);
B_Iterator B_ContainerD_bytesD___iter__ (B_ContainerD_bytes wit, B_bytes s);
B_bool B_ContainerD_bytesD___contains__ (B_ContainerD_bytes wit, B_bytes s, B_int sub);
B_bool B_ContainerD_bytesD___containsnot__ (B_ContainerD_bytes wit, B_bytes s, B_int sub);
B_int B_SliceableD_bytesD___getitem__ (B_SliceableD_bytes wit, B_bytes s, B_int i);
B_NoneType B_SliceableD_bytesD___setitem__ (B_SliceableD_bytes wit, B_bytes s, B_int i, B_int val);
B_NoneType B_SliceableD_bytesD___delitem__ (B_SliceableD_bytes wit, B_bytes s, B_int i);
B_bytes B_SliceableD_bytesD___getslice__ (B_SliceableD_bytes wit, B_bytes self, B_slice slc);
B_NoneType B_SliceableD_bytesD___setslice__ (B_SliceableD_bytes wit,  B_bytes self, B_Iterable wit2, B_slice slc, $WORD iter);
B_NoneType B_SliceableD_bytesD___delslice__ (B_SliceableD_bytes wit,  B_bytes self, B_slice slc);
B_bytes B_TimesD_bytesD___add__ (B_TimesD_bytes wit, B_bytes a, B_bytes b);
// B_bytes B_TimesD_bytesD___iadd__ (B_TimesD_bytes wit, B_bytes a, B_bytes b);
B_bytes B_TimesD_bytesD___zero__ (B_TimesD_bytes wit);
B_bytes B_TimesD_bytesD___mul__ (B_TimesD_bytes wit, B_bytes a, B_int n);
// B_bytes B_TimesD_bytesD___imul__ (B_TimesD_bytes wit, B_bytes a, B_int n);
