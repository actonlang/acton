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
unsigned char *fromB_bytes(B_bytes b);


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
