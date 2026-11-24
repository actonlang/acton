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

// The str of a str literal, for generated code: nbytes of valid UTF-8 that
// decode to nchars characters, in static storage and followed by a NUL. The
// compiler has decoded the literal so no additional checks or counts are
// needed. Like actStrFromCString it keeps no copy.
B_str actStrFromLiteral(const char *str, int64_t nbytes, int64_t nchars);

// A NUL-terminated copy of s, for a C API that takes a C string. Such an API
// takes a NUL as the end of the string, so if s contains one this raises
// nul_error instead.
const char *actStrToCString(B_str s, B_ValueError nul_error);

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
    bool (*__next__)(B_IteratorD_str, $WORD *);
};

struct B_IteratorD_str {
    struct B_IteratorD_strG_class *$class;
    B_str src;
    int64_t nxt;
};

extern struct  B_IteratorD_strG_class  B_IteratorD_strG_methods;
B_IteratorD_str B_IteratorD_strG_new(B_str);
B_Iterator B_str_iter_init(B_IteratorD_str iter, B_str src);

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
    bool (*__next__)(B_IteratorD_bytearray, $WORD *);
};

struct B_IteratorD_bytearray {
    struct B_IteratorD_bytearrayG_class *$class;
    B_bytearray src;
    int64_t nxt;
};

extern struct  B_IteratorD_bytearrayG_class  B_IteratorD_bytearrayG_methods;
B_IteratorD_bytearray B_IteratorD_bytearrayG_new(B_bytearray);
B_Iterator B_bytearray_iter_init(B_IteratorD_bytearray iter, B_bytearray src);

// bytes /////////////////////////////////////////////////////////////////////////////////////


struct B_bytes {
    struct B_bytesG_class *$class;
    int64_t nbytes;
    unsigned char *str;      // nbytes bytes, not NUL-terminated
};

// The unadorned constructors borrow immutable storage that must live as long
// as the result. Use Copy for temporary or mutable input. The Length variants
// take an exact byte count, including embedded NUL bytes, without a terminator.
B_bytes actBytesFromCString(const char *str);
B_bytes actBytesFromCStringCopy(const char *str);
B_bytes actBytesFromCStringLength(const char *str, int64_t len);
B_bytes actBytesFromCStringLengthCopy(const char *str, int64_t len);

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
    bool (*__next__)(B_IteratorD_bytes, $WORD *);
};

struct B_IteratorD_bytes {
    struct B_IteratorD_bytesG_class *$class;
    B_bytes src;
    int64_t nxt;
};

extern struct  B_IteratorD_bytesG_class  B_IteratorD_bytesG_methods;
B_IteratorD_bytes B_IteratorD_bytesG_new(B_bytes);
B_Iterator B_bytes_iter_init(B_IteratorD_bytes iter, B_bytes src);

// Internal auxiliary function /////////////////////////////////////////////

// used in defining __str__ method for collection types (list, dict, set)
B_str B_strD_join_par(char lpar,B_list elems, char rpar);

B_str $default__str__(B_value);

// Raw indexing workers used when the compiler resolves a builtin witness.
B_str B_str_getitem(B_str s, int64_t i);
int64_t B_bytes_getitem(B_bytes str, int64_t i);
int64_t B_bytearray_getitem(B_bytearray self, int64_t i);
B_NoneType B_bytearray_setitem(B_bytearray self, int64_t i, int64_t val);
B_NoneType B_bytearray_delitem(B_bytearray self, int64_t i);

// Witness-free length getters used by generated code.
static inline int64_t B_str_len(B_str self) {
    return (int64_t)self->nchars;
}
static inline int64_t B_bytes_len(B_bytes self) {
    return (int64_t)self->nbytes;
}
static inline int64_t B_bytearray_len(B_bytearray self) {
    return (int64_t)self->nbytes;
}

// Witness-free string operations used by generated code and protocol wrappers.
static inline bool B_str_eq(B_str a, B_str b) {
    if (a == b)
        return true;
    return a->nbytes == b->nbytes && memcmp(a->str, b->str, a->nbytes) == 0;
}
B_str B_str_add(B_str s, B_str t);

// Native string and byte-container operations shared with protocol wrappers.
int B_str_compare(B_str a, B_str b);
B_NoneType B_str_hash(B_str a, B_hasher h);
B_str B_str_zero(void);
B_str B_str_mul(B_str a, int64_t n);
B_str B_str_fromiter(B_Iterable wit2, $WORD iter);
bool B_str_contains(B_str s, B_str sub);
B_Iterator B_str_iter(B_str s);
B_str B_str_getslice(B_str s, B_slice slc);
bool B_bytes_eq(B_bytes a, B_bytes b);
bool B_bytes_lt(B_bytes a, B_bytes b);
bool B_bytes_contains(B_bytes str, int64_t n);
bool B_bytearray_eq(B_bytearray a, B_bytearray b);
bool B_bytearray_lt(B_bytearray a, B_bytearray b);
bool B_bytearray_contains(B_bytearray self, int64_t n);
B_NoneType B_bytes_hash(B_bytes a, B_hasher h);
B_Iterator B_bytes_iter(B_bytes str);
B_bytes B_bytes_getslice(B_bytes str, B_slice slc);
B_bytes B_bytes_add(B_bytes s, B_bytes t);
B_bytes B_bytes_zero(void);
B_bytes B_bytes_mul(B_bytes a, int64_t n);
B_NoneType B_bytearray_insert(B_bytearray self, int64_t n, int64_t elem);
B_NoneType B_bytearray_append(B_bytearray self, int64_t elem);
B_NoneType B_bytearray_reverse(B_bytearray self);
B_Iterator B_bytearray_reversed(B_bytearray self);
B_bytearray B_bytearray_getslice(B_bytearray self, B_slice slc);
B_NoneType B_bytearray_setslice(B_bytearray self, B_Iterable wit2, B_slice slc, $WORD iter);
B_NoneType B_bytearray_delslice(B_bytearray self, B_slice slc);
B_bytearray B_bytearray_fromiter(B_Iterable wit2, $WORD iter);
B_Iterator B_bytearray_iter(B_bytearray str);
B_bytearray B_bytearray_add(B_bytearray a, B_bytearray b);
B_bytearray B_bytearray_zero(void);
B_bytearray B_bytearray_mul(B_bytearray a, int64_t n);
B_bytearray B_bytearray_iadd(B_bytearray a, B_bytearray b);
B_bytearray B_bytearray_imul(B_bytearray a, int64_t n);

// Witness-free formatting and truth workers.
bool B_str_bool(B_str self);
B_str B_str_str(B_str self);
B_str B_str_repr(B_str self);
bool B_bytes_bool(B_bytes self);
B_str B_bytes_str(B_bytes self);
B_str B_bytes_repr(B_bytes self);
bool B_bytearray_bool(B_bytearray self);
B_str B_bytearray_str(B_bytearray self);
B_str B_bytearray_repr(B_bytearray self);

// Normalize a formatter result to the base str class.
B_str B_str_from_str(B_str result);
