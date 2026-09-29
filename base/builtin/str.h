struct B_strG_class;

// The longest str, bytes or bytearray, in bytes. It is far more than any
// machine has memory for, and small enough that up to 8 lengths of at most
// this much add up without overflowing an int64_t.
#define MAX_STR_LEN (INT64_MAX / 8)

struct B_str {
    struct B_strG_class *$class;
    int64_t nbytes;          // length of str in bytes
    int64_t nchars;          // length of str in Unicode chars
    unsigned char *str;      // str is UTF-8 encoded.
};

// Decode UTF-8, raising ValueError for invalid input. The Length variants
// preserve embedded NUL and take a byte count, excluding the final terminator.
// Without Copy, the input must remain immutable and alive for the result's
// lifetime: use static storage or retained Acton GC storage, never a stack
// buffer or memory that will be freed. Length also requires str[nbytes] == 0.
// Copy accepts temporary buffers; LengthCopy does not require a terminator.
// Empty strings and one-byte ASCII strings may reuse immutable singletons.
B_str actStrFromCString(const char *str);
B_str actStrFromCStringCopy(const char *str);
B_str actStrFromCStringLength(const char *str, int64_t nbytes);
B_str actStrFromCStringLengthCopy(const char *str, int64_t nbytes);

// A NUL-terminated copy of s, for a C API that takes a C string. Such an API
// takes a NUL as the end of the string, so if s contains one this raises
// nul_error instead.
const char *actStrToCString(B_str s, B_ValueError nul_error);

// Compatibility names. Preserve their original copying behaviour until
// downstream callers have migrated to the explicit APIs above.
B_str to$str(char *str);                         // Copies.
B_str to_str_len(const char *str, int64_t nbytes);   // Copies; preserves embedded NUL.
B_str to_str_noc(char *str);                     // Wraps the input without copying.
B_str toB_str(char *str);

// Destructor; recover the internal string.
unsigned char *fromB_str(B_str str);

B_str $FORMAT(const char *format, ...);

// Format for the % operator and interpolated strings: the format is nbytes long
// and %s, %r and %a take a B_str, so NUL bytes survive (see str.c).
B_str $FORMAT_len(const char *format, int64_t nbytes, ...);

// Find byte position in text from char position.
// Assume i is a valid char index in text
int64_t $byte_no(B_str text, int64_t i);

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
int64_t $char_no(B_str text, int64_t i);

// Iterators over str's ///////////////////////////////////////////////////////

typedef struct B_IteratorD_str *B_IteratorD_str; ;

struct B_IteratorD_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorD_str, B_str);
    void (*__serialize__)(B_IteratorD_str,$Serial$state);
    B_IteratorD_str (*__deserialize__)(B_IteratorD_str,$Serial$state);
    bool (*__bool__)(B_IteratorD_str);
    B_str (*__str__)(B_IteratorD_str);
    B_str (*__repr__)(B_IteratorD_str);
    bool (*__next__)(B_IteratorD_str, $WORD *);
};

struct B_IteratorD_str {
    struct B_IteratorD_strG_class *$class;
    B_str src;
    int64_t nxt;
};

extern struct  B_IteratorD_strG_class  B_IteratorD_strG_methods;
B_IteratorD_str B_IteratorD_strG_new(B_str);

// bytearray /////////////////////////////////////////////////////////////////////////////////////



struct B_bytearray {
    struct B_bytearrayG_class *$class;
    int64_t nbytes;
    unsigned char *str;      // nbytes bytes, not NUL-terminated
    int64_t capacity;
};

 
B_bytearray toB_bytearray(char *str); 
unsigned char *fromB_bytearray(B_bytearray b);

// Iterators over bytearrays ///////////////////////////////////////////////////////

typedef struct B_IteratorD_bytearray *B_IteratorD_bytearray; ;

struct B_IteratorD_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorD_bytearray, B_bytearray);
    void (*__serialize__)(B_IteratorD_bytearray,$Serial$state);
    B_IteratorD_bytearray (*__deserialize__)(B_IteratorD_bytearray,$Serial$state);
    bool (*__bool__)(B_IteratorD_bytearray);
    B_str (*__str__)(B_IteratorD_bytearray);
    B_str (*__repr__)(B_IteratorD_bytearray);
    bool (*__next__)(B_IteratorD_bytearray, $WORD *);
};

struct B_IteratorD_bytearray {
    struct B_IteratorD_bytearrayG_class *$class;
    B_bytearray src;
    int64_t nxt;
};

extern struct  B_IteratorD_bytearrayG_class  B_IteratorD_bytearrayG_methods;
B_IteratorD_bytearray B_IteratorD_bytearrayG_new(B_bytearray);

// bytes /////////////////////////////////////////////////////////////////////////////////////


struct B_bytes {
    struct B_bytesG_class *$class;
    int64_t nbytes;
    unsigned char *str;      // nbytes bytes, not NUL-terminated
};

// Copy into independent storage. LengthCopy preserves embedded NUL and does
// not require a terminator; the count excludes any final terminator.
B_bytes actBytesFromCStringCopy(const char *str);
B_bytes actBytesFromCStringLengthCopy(const char *str, int64_t len);

// Existing APIs retain their behaviour during the downstream migration:
// the unadorned names still copy. NoCopy requires immutable storage that
// remains alive for the result's lifetime.
B_bytes actBytesFromCString(char *str);
B_bytes actBytesFromCStringNoCopy(char *str);
B_bytes actBytesFromCStringLength(char *str, int64_t len);
B_bytes actBytesFromCStringLengthNoCopy(char *str, int64_t length);
B_bytes to$bytes(char *str);
B_bytes to$bytesD_len(char *str, int64_t len);
char *fromB_bytes(B_bytes b);


// Iterators over bytes ///////////////////////////////////////////////////////


typedef struct B_IteratorD_bytes *B_IteratorD_bytes; ;

struct B_IteratorD_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorD_bytes, B_bytes);
    void (*__serialize__)(B_IteratorD_bytes,$Serial$state);
    B_IteratorD_bytes (*__deserialize__)(B_IteratorD_bytes,$Serial$state);
    bool (*__bool__)(B_IteratorD_bytes);
    B_str (*__str__)(B_IteratorD_bytes);
    B_str (*__repr__)(B_IteratorD_bytes);
    bool (*__next__)(B_IteratorD_bytes, $WORD *);
};

struct B_IteratorD_bytes {
    struct B_IteratorD_bytesG_class *$class;
    B_bytes src;
    int64_t nxt;
};

extern struct  B_IteratorD_bytesG_class  B_IteratorD_bytesG_methods;
B_IteratorD_bytes B_IteratorD_bytesG_new(B_bytes);

// Internal auxiliary function /////////////////////////////////////////////

// used in defining __str__ method for collection types (list, dict, set)
B_str B_strD_join_par(char lpar,B_list elems, char rpar);

B_str $default__str__(B_value);

// Raw indexing workers used when the compiler resolves a builtin witness.
B_str $strD_U__getitem__(B_str s, int64_t i);
int64_t $bytesD_U__getitem__(B_bytes str, int64_t i);
int64_t $bytearrayD_U__getitem__(B_bytearray self, int64_t i);
B_NoneType $bytearrayD_U__setitem__(B_bytearray self, int64_t i, int64_t val);
B_NoneType $bytearrayD_U__delitem__(B_bytearray self, int64_t i);
