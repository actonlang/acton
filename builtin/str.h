struct B_strG_class;

struct B_str {
    struct B_strG_class *$class;
    int nbytes;              // length of str in bytes
    int nchars;              // length of str in Unicode chars
    unsigned char *str;      // str is UTF-8 encoded.
};

struct B_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_str, B_value);
    void (*__serialize__)(B_str,$NoneType);
    B_str (*__deserialize__)(B_str,$NoneType);
    B_bool (*__bool__)(B_str);
    B_str (*__str__)(B_str);
    B_str (*__repr__)(B_str);
    B_str (*capitalize)(B_str s);
    B_str (*center)(B_str s, B_int width, B_str fill);                 // raises TYPEERROR if fill is not a single char
    B_int (*count)(B_str s, B_str sub, B_int start, B_int end);
    B_bytes (*encode)(B_str s);                                    // only utf-8 encoding and strict error handling
    B_bool (*endswith)(B_str s, B_str suffix, B_int start, B_int end);
    B_str (*expandtabs)(B_str s, B_int tabsize);     
    B_int (*find)(B_str s, B_str sub, B_int start, B_int end);         // returns -1 when not found
    B_int (*index)(B_str s, B_str sub, B_int start, B_int end);        // like find but raises VALUEERROR when not found
    B_bool (*isalnum)(B_str s);                                     // not exactly as in Python; all chars c satisfy isalpha(c) or isdecimal(c)
    B_bool (*isalpha)(B_str s);
    B_bool (*isascii)(B_str s);
    B_bool (*isdecimal)(B_str s);
    //B_bool (*isdigit)(B_str s);                                    // not implemented; relies on property NT(numeric_type)
    //B_bool (*isidentifier)(B_str s);                               // not implemented
    B_bool (*islower)(B_str s);
    //B_bool (*isnumeric)(B_str s);                                  // not implemented; relies on property NT(numeric_type)
    B_bool (*isprintable)(B_str s);
    B_bool (*isspace)(B_str s);
    B_bool (*istitle)(B_str s);
    B_bool (*isupper)(B_str s);
    B_str (*join)(B_str sep, B_Iterable wit, $WORD iter);
    B_str (*ljust)(B_str s, B_int width, B_str fill);                   // raises TYPEERROR if fill is not a single char
    B_str (*lower)(B_str s);
    B_str (*lstrip)(B_str s,B_str cs);                                // cs may be NULL, then defaulting to whitespace removal.
    //maketrans not implemented
    B_tuple (*partition)(B_str s, B_str sep);
    B_str (*replace)(B_str s, B_str old, B_str new, B_int count);
    B_int (*rfind)(B_str s, B_str sub, B_int start, B_int end);         // returns -1 when not found
    B_int (*rindex)(B_str s, B_str sub, B_int start, B_int end);        // like rfind but raises VALUEERROR when not found
    B_str (*rjust)(B_str s, B_int width, B_str fill);                   // raises TYPEERROR if fill is not a single char
    B_tuple (*rpartition)(B_str s, B_str sep); 
    //B_list (*rsplit)(B_str s, B_str sep, int maxsplit);             // not implemented. sep may be NULL; then separation is indicated by a whitespace string. TODO!!!
    B_str (*rstrip)(B_str s,B_str cs);                                //  cs may be NULL, then defaulting to whitespace removal.
    B_list (*split)(B_str s, B_str sep, B_int maxsplit);               // raises VALUEERROR when separator is empty string
    B_list (*splitlines)(B_str s, B_bool);                                   // keepends parameter absent; only \n recognized as line separator
    B_bool (*startswith)(B_str s, B_str prefix, B_int start, B_int end); 
    B_str (*strip)(B_str s, B_str cs);                                // cs may be NULL, then defaulting to whitespace removal.
    // translate not implemented
    B_str (*upper)(B_str s);
    B_str (*zfill)(B_str s, B_int width);
};

extern struct B_strG_class B_strG_methods;
B_str B_strG_new(B_value);

extern struct B_OrdD_strG_class B_OrdD_strG_methods;
B_OrdD_str B_OrdD_strG_new();
extern struct B_HashableD_strG_class B_HashableD_strG_methods;
B_HashableD_str B_HashableD_strG_new();
extern struct B_TimesD_strG_class B_TimesD_strG_methods;
B_TimesD_str B_TimesD_strG_new();
extern struct B_SliceableD_strG_class B_SliceableD_strG_methods;
B_SliceableD_str B_SliceableD_strG_new();
extern struct B_ContainerD_strG_class B_ContainerD_strG_methods;
B_ContainerD_str B_ContainerD_strG_new();

extern struct B_OrdD_str *B_OrdD_strG_witness;
extern struct B_HashableD_str *B_HashableD_strG_witness;
extern struct B_TimesD_str *B_TimesD_strG_witness;
extern struct B_SliceableD_str *B_SliceableD_strG_witness;
extern struct B_ContainerD_str *B_ContainerD_strG_witness;

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a B_str value.
B_str to$str(char *str);
// Destructor; recover the internal string.
unsigned char *fromB_str(B_str str);

// Iterators over str's ///////////////////////////////////////////////////////

typedef struct B_IteratorB_str *B_IteratorB_str; ;

struct B_IteratorB_strG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_str, B_str);
    void (*__serialize__)(B_IteratorB_str,$NoneType);
    B_IteratorB_str (*__deserialize__)(B_IteratorB_str,$NoneType);
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


struct B_bytearrayG_class;

struct B_bytearray {
    struct B_bytearrayG_class *$class;
    int nbytes;
    int capacity;
    unsigned char *str;
};

struct B_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_bytearray, B_bytes);
    void (*__serialize__)(B_bytearray,$NoneType);
    B_bytearray (*__deserialize__)(B_bytearray,$NoneType);
    B_bool (*__bool__)(B_bytearray);
    B_str (*__str__)(B_bytearray);
    B_str (*__repr__)(B_bytearray);
    B_bytearray (*capitalize)(B_bytearray s);
    B_bytearray (*center)(B_bytearray s, B_int width, B_bytearray fill);                 
    B_int (*count)(B_bytearray s, B_bytearray sub, B_int start, B_int end);
    B_str (*decode)(B_bytearray);
    B_bool (*endswith)(B_bytearray s, B_bytearray suffix, B_int start, B_int end);
    B_bytearray (*expandtabs)(B_bytearray s, B_int tabsize);     
    B_int (*find)(B_bytearray s, B_bytearray sub, B_int start, B_int end);         
    B_int (*index)(B_bytearray s, B_bytearray sub, B_int start, B_int end);        
    B_bool (*isalnum)(B_bytearray s);                                     
    B_bool (*isalpha)(B_bytearray s);
    B_bool (*isascii)(B_bytearray s);
    B_bool (*isdigit)(B_bytearray s);
    B_bool (*islower)(B_bytearray s);
    //  B_bool (*isprintable)(B_bytearray s);
    B_bool (*isspace)(B_bytearray s);
    B_bool (*istitle)(B_bytearray s);
    B_bool (*isupper)(B_bytearray s);
    B_bytearray (*join)(B_bytearray sep, B_Iterable wit, $WORD iter);
    B_bytearray (*ljust)(B_bytearray s, B_int width, B_bytearray fill);                  
    B_bytearray (*lower)(B_bytearray s);
    B_bytearray (*lstrip)(B_bytearray s,B_bytearray cs);                               
    B_tuple (*partition)(B_bytearray s, B_bytearray sep);
    B_bytearray (*replace)(B_bytearray s, B_bytearray old, B_bytearray new, B_int count);
    B_int (*rfind)(B_bytearray s, B_bytearray sub, B_int start, B_int end);
    B_int (*rindex)(B_bytearray s, B_bytearray sub, B_int start, B_int end);       
    B_bytearray (*rjust)(B_bytearray s, B_int width, B_bytearray fill);                  
    B_tuple (*rpartition)(B_bytearray s, B_bytearray sep); 
    //B_list (*rsplit)(B_bytearray s, B_bytearray sep, int maxsplit);             
    B_bytearray (*rstrip)(B_bytearray s,B_bytearray cs);                                
    B_list (*split)(B_bytearray s, B_bytearray sep, B_int maxsplit);               
    B_list (*splitlines)(B_bytearray s, B_bool keepends);                                   
    B_bool (*startswith)(B_bytearray s, B_bytearray prefix, B_int start, B_int end);
    B_bytearray (*strip)(B_bytearray s, B_bytearray cs);                                
    B_bytearray (*upper)(B_bytearray s);
    B_bytearray (*zfill)(B_bytearray s, B_int width);
};

extern struct B_bytearrayG_class B_bytearrayG_methods;
B_bytearray B_bytearrayG_new(B_bytes);

extern struct B_OrdD_bytearrayG_class B_OrdD_bytearrayG_methods;
B_OrdD_bytearray B_OrdD_bytearrayG_new();
extern struct B_SequenceD_bytearrayG_class B_SequenceD_bytearrayG_methods;
B_SequenceD_bytearray B_SequenceD_bytearrayG_new();
extern struct B_CollectionD_SequenceD_bytearrayG_class B_CollectionD_SequenceD_bytearrayG_methods;
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_new(B_Sequence);
extern struct B_TimesD_SequenceD_bytearrayG_class B_TimesD_SequenceD_bytearrayG_methods;
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_new(B_Sequence);
extern struct B_ContainerD_bytearrayG_class B_ContainerD_bytearrayG_methods;
B_ContainerD_bytearray B_ContainerD_bytearrayG_new();

extern struct B_OrdD_bytearray *B_OrdD_bytearrayG_witness;
extern struct B_SequenceD_bytearray *B_SequenceD_bytearrayG_witness;
extern struct B_CollectionD_SequenceD_bytearray *B_CollectionD_SequenceD_bytearrayG_witness;
extern struct B_TimesD_SequenceD_bytearray *B_TimesD_SequenceD_bytearrayG_witness;
extern struct B_ContainerD_bytearray *B_ContainerD_bytearrayG_witness;

B_bytearray toB_bytearray(char *str); 
unsigned char *fromB_bytearray(B_bytearray b);

// Iterators over bytearrays ///////////////////////////////////////////////////////

typedef struct B_IteratorB_bytearray *B_IteratorB_bytearray; ;

struct B_IteratorB_bytearrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_bytearray, B_bytearray);
    void (*__serialize__)(B_IteratorB_bytearray,$NoneType);
    B_IteratorB_bytearray (*__deserialize__)(B_IteratorB_bytearray,$NoneType);
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


struct B_bytesG_class;

struct B_bytes {
    struct B_bytesG_class *$class;
    int nbytes;
    unsigned char *str;
};

struct B_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_bytes, B_Iterable, $WORD);
    void (*__serialize__)(B_bytes,$NoneType);
    B_bytes (*__deserialize__)(B_bytes,$NoneType);
    B_bool (*__bool__)(B_bytes);
    B_str (*__str__)(B_bytes);
    B_str (*__repr__)(B_bytes);
    B_bytes (*capitalize)(B_bytes s);
    B_bytes (*center)(B_bytes s, B_int width, B_bytes fill);                 
    B_int (*count)(B_bytes s, B_bytes sub, B_int start, B_int end);
    B_str (*decode)(B_bytes);
    B_bool (*endswith)(B_bytes s, B_bytes suffix, B_int start, B_int end);
    B_bytes (*expandtabs)(B_bytes s, B_int tabsize);     
    B_int (*find)(B_bytes s, B_bytes sub, B_int start, B_int end);         
    B_int (*index)(B_bytes s, B_bytes sub, B_int start, B_int end);        
    B_bool (*isalnum)(B_bytes s);                                     
    B_bool (*isalpha)(B_bytes s);
    B_bool (*isascii)(B_bytes s);
    B_bool (*isdigit)(B_bytes s);
    B_bool (*islower)(B_bytes s);
    //  B_bool (*isprintable)(B_bytes s);
    B_bool (*isspace)(B_bytes s);
    B_bool (*istitle)(B_bytes s);
    B_bool (*isupper)(B_bytes s);
    B_bytes (*join)(B_bytes sep, B_Iterable wit, $WORD iter);
    B_bytes (*ljust)(B_bytes s, B_int width, B_bytes fill);                  
    B_bytes (*lower)(B_bytes s);
    B_bytes (*lstrip)(B_bytes s,B_bytes cs);                               
    B_tuple (*partition)(B_bytes s, B_bytes sep);
    B_bytes (*replace)(B_bytes s, B_bytes old, B_bytes new, B_int count);
    B_int (*rfind)(B_bytes s, B_bytes sub, B_int start, B_int end);
    B_int (*rindex)(B_bytes s, B_bytes sub, B_int start, B_int end);       
    B_bytes (*rjust)(B_bytes s, B_int width, B_bytes fill);                  
    B_tuple (*rpartition)(B_bytes s, B_bytes sep); 
    //B_list (*rsplit)(B_bytes s, B_bytes sep, int maxsplit);             
    B_bytes (*rstrip)(B_bytes s,B_bytes cs);                                
    B_list (*split)(B_bytes s, B_bytes sep, B_int maxsplit);               
    B_list (*splitlines)(B_bytes s, B_bool keepends);                                   
    B_bool (*startswith)(B_bytes s, B_bytes prefix, B_int start, B_int end);
    B_bytes (*strip)(B_bytes s, B_bytes cs);                                
    B_bytes (*upper)(B_bytes s);
    B_bytes (*zfill)(B_bytes s, B_int width);
};

extern struct B_bytesG_class B_bytesG_methods;
B_bytes B_bytesG_new(B_Iterable,$WORD);

extern struct B_OrdD_bytesG_class B_OrdD_bytesG_methods;
B_OrdD_bytes B_OrdD_bytesG_new();
extern struct B_HashableD_bytesG_class B_HashableD_bytesG_methods;
B_HashableD_bytes B_HashableD_bytesG_new();
extern struct B_TimesD_bytesG_class B_TimesD_bytesG_methods;
B_TimesD_bytes B_TimesD_bytesG_new();
extern struct B_SliceableD_bytesG_class B_SliceableD_bytesG_methods;
B_SliceableD_bytes B_SliceableD_bytesG_new();
extern struct B_ContainerD_bytesG_class B_ContainerD_bytesG_methods;
B_ContainerD_bytes B_ContainerD_bytesG_new();

extern struct B_OrdD_bytes *B_OrdD_bytesG_witness;
extern struct B_HashableD_bytes *B_HashableD_bytesG_witness;
extern struct B_TimesD_bytes *B_TimesD_bytesG_witness;
extern struct B_SliceableD_bytes *B_SliceableD_bytesG_witness;
extern struct B_ContainerD_bytes *B_ContainerD_bytesG_witness;

B_bytes toB_bytes(char *str);
B_bytes toB_bytesD_len(char *str, int len);
unsigned char *fromB_bytes(B_bytes b);


// Iterators over bytess ///////////////////////////////////////////////////////


typedef struct B_IteratorB_bytes *B_IteratorB_bytes; ;

struct B_IteratorB_bytesG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_bytes, B_bytes);
    void (*__serialize__)(B_IteratorB_bytes,$NoneType);
    B_IteratorB_bytes (*__deserialize__)(B_IteratorB_bytes,$NoneType);
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

//builtin functions //////////////////////////////////////////////////////////////////////////////////

// Backslash, single and double quote are always escaped.
// All control and non-ASCII bytes are escaped using \xhh (so \x0a is used instead of \n, etc)
// Single quotes are used as string delimiters.
B_str $ascii(B_str s);
B_str $bin(B_Integral wit, $WORD n);
B_str $chr(B_Integral wit, $WORD n);
B_str $hex(B_Integral wit, $WORD n);
B_int $ord(B_str c);


// Internal auxiliary function /////////////////////////////////////////////

// used in defining __str__ method for collection types (list, dict, set)
B_str B_strD_join_par(char lpar,B_list elems, char rpar);

B_str $default__str__(B_value);
