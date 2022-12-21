struct $str$class;

struct $str {
    struct $str$class *$class;
    int nbytes;              // length of str in bytes
    int nchars;              // length of str in Unicode chars
    unsigned char *str;      // str is UTF-8 encoded.
};

struct $str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($str, $value);
    void (*__serialize__)($str,$Serial$state);
    $str (*__deserialize__)($str,$Serial$state);
    $bool (*__bool__)($str);
    $str (*__str__)($str);
    $str (*__repr__)($str);
    $str (*capitalize)($str s);
    $str (*center)($str s, $int width, $str fill);                 // raises TYPEERROR if fill is not a single char
    $int (*count)($str s, $str sub, $int start, $int end);
    $bytes (*encode)($str s);                                    // only utf-8 encoding and strict error handling
    $bool (*endswith)($str s, $str suffix, $int start, $int end);
    $str (*expandtabs)($str s, $int tabsize);     
    $int (*find)($str s, $str sub, $int start, $int end);         // returns -1 when not found
    $int (*index)($str s, $str sub, $int start, $int end);        // like find but raises VALUEERROR when not found
    $bool (*isalnum)($str s);                                     // not exactly as in Python; all chars c satisfy isalpha(c) or isdecimal(c)
    $bool (*isalpha)($str s);
    $bool (*isascii)($str s);
    $bool (*isdecimal)($str s);
    //$bool (*isdigit)($str s);                                    // not implemented; relies on property NT(numeric_type)
    //$bool (*isidentifier)($str s);                               // not implemented
    $bool (*islower)($str s);
    //$bool (*isnumeric)($str s);                                  // not implemented; relies on property NT(numeric_type)
    $bool (*isprintable)($str s);
    $bool (*isspace)($str s);
    $bool (*istitle)($str s);
    $bool (*isupper)($str s);
    $str (*join)($str sep, $Iterable wit, $WORD iter);
    $str (*ljust)($str s, $int width, $str fill);                   // raises TYPEERROR if fill is not a single char
    $str (*lower)($str s);
    $str (*lstrip)($str s,$str cs);                                // cs may be NULL, then defaulting to whitespace removal.
    //maketrans not implemented
    $tuple (*partition)($str s, $str sep);
    $str (*replace)($str s, $str old, $str new, $int count);
    $int (*rfind)($str s, $str sub, $int start, $int end);         // returns -1 when not found
    $int (*rindex)($str s, $str sub, $int start, $int end);        // like rfind but raises VALUEERROR when not found
    $str (*rjust)($str s, $int width, $str fill);                   // raises TYPEERROR if fill is not a single char
    $tuple (*rpartition)($str s, $str sep); 
    //$list (*rsplit)($str s, $str sep, int maxsplit);             // not implemented. sep may be NULL; then separation is indicated by a whitespace string. TODO!!!
    $str (*rstrip)($str s,$str cs);                                //  cs may be NULL, then defaulting to whitespace removal.
    $list (*split)($str s, $str sep, $int maxsplit);               // raises VALUEERROR when separator is empty string
    $list (*splitlines)($str s, $bool);                                   // keepends parameter absent; only \n recognized as line separator
    $bool (*startswith)($str s, $str prefix, $int start, $int end); 
    $str (*strip)($str s, $str cs);                                // cs may be NULL, then defaulting to whitespace removal.
    // translate not implemented
    $str (*upper)($str s);
    $str (*zfill)($str s, $int width);
};

extern struct $str$class $str$methods;
$str $str$new($value);

extern struct $Ord$str$class $Ord$str$methods;
$Ord$str $Ord$str$new();
extern struct $Hashable$str$class $Hashable$str$methods;
$Hashable$str $Hashable$str$new();
extern struct $Times$str$class $Times$str$methods;
$Times$str $Times$str$new();
extern struct $Sliceable$str$class $Sliceable$str$methods;
$Sliceable$str $Sliceable$str$new();
extern struct $Container$str$class $Container$str$methods;
$Container$str $Container$str$new();

extern struct $Ord$str *$Ord$str$witness;
extern struct $Hashable$str *$Hashable$str$witness;
extern struct $Times$str *$Times$str$witness;
extern struct $Sliceable$str *$Sliceable$str$witness;
extern struct $Container$str *$Container$str$witness;

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a $str value.
$str to$str(char *str);
// Destructor; recover the internal string.
unsigned char *from$str($str str);

// Iterators over str's ///////////////////////////////////////////////////////

typedef struct $Iterator$str *$Iterator$str; ;

struct $Iterator$str$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterator$str, $str);
    void (*__serialize__)($Iterator$str,$Serial$state);
    $Iterator$str (*__deserialize__)($Iterator$str,$Serial$state);
    $bool (*__bool__)($Iterator$str);
    $str (*__str__)($Iterator$str);
    $str (*__repr__)($Iterator$str);
    $str (*__next__)($Iterator$str);
};

struct $Iterator$str {
    struct $Iterator$str$class *$class;
    $str src;
    int nxt;
};

extern struct  $Iterator$str$class  $Iterator$str$methods;
$Iterator$str $Iterator$str$new($str);

// bytearray /////////////////////////////////////////////////////////////////////////////////////


struct $bytearray$class;

struct $bytearray {
    struct $bytearray$class *$class;
    int nbytes;
    int capacity;
    unsigned char *str;
};

struct $bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($bytearray, $bytes);
    void (*__serialize__)($bytearray,$Serial$state);
    $bytearray (*__deserialize__)($bytearray,$Serial$state);
    $bool (*__bool__)($bytearray);
    $str (*__str__)($bytearray);
    $str (*__repr__)($bytearray);
    $bytearray (*capitalize)($bytearray s);
    $bytearray (*center)($bytearray s, $int width, $bytearray fill);                 
    $int (*count)($bytearray s, $bytearray sub, $int start, $int end);
    $str (*decode)($bytearray);
    $bool (*endswith)($bytearray s, $bytearray suffix, $int start, $int end);
    $bytearray (*expandtabs)($bytearray s, $int tabsize);     
    $int (*find)($bytearray s, $bytearray sub, $int start, $int end);         
    $int (*index)($bytearray s, $bytearray sub, $int start, $int end);        
    $bool (*isalnum)($bytearray s);                                     
    $bool (*isalpha)($bytearray s);
    $bool (*isascii)($bytearray s);
    $bool (*isdigit)($bytearray s);
    $bool (*islower)($bytearray s);
    //  $bool (*isprintable)($bytearray s);
    $bool (*isspace)($bytearray s);
    $bool (*istitle)($bytearray s);
    $bool (*isupper)($bytearray s);
    $bytearray (*join)($bytearray sep, $Iterable wit, $WORD iter);
    $bytearray (*ljust)($bytearray s, $int width, $bytearray fill);                  
    $bytearray (*lower)($bytearray s);
    $bytearray (*lstrip)($bytearray s,$bytearray cs);                               
    $tuple (*partition)($bytearray s, $bytearray sep);
    $bytearray (*replace)($bytearray s, $bytearray old, $bytearray new, $int count);
    $int (*rfind)($bytearray s, $bytearray sub, $int start, $int end);
    $int (*rindex)($bytearray s, $bytearray sub, $int start, $int end);       
    $bytearray (*rjust)($bytearray s, $int width, $bytearray fill);                  
    $tuple (*rpartition)($bytearray s, $bytearray sep); 
    //$list (*rsplit)($bytearray s, $bytearray sep, int maxsplit);             
    $bytearray (*rstrip)($bytearray s,$bytearray cs);                                
    $list (*split)($bytearray s, $bytearray sep, $int maxsplit);               
    $list (*splitlines)($bytearray s, $bool keepends);                                   
    $bool (*startswith)($bytearray s, $bytearray prefix, $int start, $int end);
    $bytearray (*strip)($bytearray s, $bytearray cs);                                
    $bytearray (*upper)($bytearray s);
    $bytearray (*zfill)($bytearray s, $int width);
};

extern struct $bytearray$class $bytearray$methods;
$bytearray $bytearray$new($bytes);

extern struct $Ord$bytearray$class $Ord$bytearray$methods;
$Ord$bytearray $Ord$bytearray$new();
extern struct $Sequence$bytearray$class $Sequence$bytearray$methods;
$Sequence$bytearray $Sequence$bytearray$new();
extern struct $Collection$bytearray$class $Collection$bytearray$methods;
$Collection$bytearray $Collection$bytearray$new($Sequence);
extern struct $Times$bytearray$class $Times$bytearray$methods;
$Times$bytearray $Times$bytearray$new($Sequence);
extern struct $Container$bytearray$class $Container$bytearray$methods;
$Container$bytearray $Container$bytearray$new();

extern struct $Ord$bytearray *$Ord$bytearray$witness;
extern struct $Sequence$bytearray *$Sequence$bytearray$witness;
extern struct $Collection$bytearray *$Collection$bytearray$witness;
extern struct $Times$bytearray *$Times$bytearray$witness;
extern struct $Container$bytearray *$Container$bytearray$witness;

$bytearray to$bytearray(char *str); 
unsigned char *from$bytearray($bytearray b);

// Iterators over bytearrays ///////////////////////////////////////////////////////

typedef struct $Iterator$bytearray *$Iterator$bytearray; ;

struct $Iterator$bytearray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterator$bytearray, $bytearray);
    void (*__serialize__)($Iterator$bytearray,$Serial$state);
    $Iterator$bytearray (*__deserialize__)($Iterator$bytearray,$Serial$state);
    $bool (*__bool__)($Iterator$bytearray);
    $str (*__str__)($Iterator$bytearray);
    $str (*__repr__)($Iterator$bytearray);
    $int (*__next__)($Iterator$bytearray);
};

struct $Iterator$bytearray {
    struct $Iterator$bytearray$class *$class;
    $bytearray src;
    int nxt;
};

extern struct  $Iterator$bytearray$class  $Iterator$bytearray$methods;
$Iterator$bytearray $Iterator$bytearray$new($bytearray);

// bytes /////////////////////////////////////////////////////////////////////////////////////


struct $bytes$class;

struct $bytes {
    struct $bytes$class *$class;
    int nbytes;
    unsigned char *str;
};

struct $bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($bytes, $Iterable, $WORD);
    void (*__serialize__)($bytes,$Serial$state);
    $bytes (*__deserialize__)($bytes,$Serial$state);
    $bool (*__bool__)($bytes);
    $str (*__str__)($bytes);
    $str (*__repr__)($bytes);
    $bytes (*capitalize)($bytes s);
    $bytes (*center)($bytes s, $int width, $bytes fill);                 
    $int (*count)($bytes s, $bytes sub, $int start, $int end);
    $str (*decode)($bytes);
    $bool (*endswith)($bytes s, $bytes suffix, $int start, $int end);
    $bytes (*expandtabs)($bytes s, $int tabsize);     
    $int (*find)($bytes s, $bytes sub, $int start, $int end);         
    $int (*index)($bytes s, $bytes sub, $int start, $int end);        
    $bool (*isalnum)($bytes s);                                     
    $bool (*isalpha)($bytes s);
    $bool (*isascii)($bytes s);
    $bool (*isdigit)($bytes s);
    $bool (*islower)($bytes s);
    //  $bool (*isprintable)($bytes s);
    $bool (*isspace)($bytes s);
    $bool (*istitle)($bytes s);
    $bool (*isupper)($bytes s);
    $bytes (*join)($bytes sep, $Iterable wit, $WORD iter);
    $bytes (*ljust)($bytes s, $int width, $bytes fill);                  
    $bytes (*lower)($bytes s);
    $bytes (*lstrip)($bytes s,$bytes cs);                               
    $tuple (*partition)($bytes s, $bytes sep);
    $bytes (*replace)($bytes s, $bytes old, $bytes new, $int count);
    $int (*rfind)($bytes s, $bytes sub, $int start, $int end);
    $int (*rindex)($bytes s, $bytes sub, $int start, $int end);       
    $bytes (*rjust)($bytes s, $int width, $bytes fill);                  
    $tuple (*rpartition)($bytes s, $bytes sep); 
    //$list (*rsplit)($bytes s, $bytes sep, int maxsplit);             
    $bytes (*rstrip)($bytes s,$bytes cs);                                
    $list (*split)($bytes s, $bytes sep, $int maxsplit);               
    $list (*splitlines)($bytes s, $bool keepends);                                   
    $bool (*startswith)($bytes s, $bytes prefix, $int start, $int end);
    $bytes (*strip)($bytes s, $bytes cs);                                
    $bytes (*upper)($bytes s);
    $bytes (*zfill)($bytes s, $int width);
};

extern struct $bytes$class $bytes$methods;
$bytes $bytes$new($Iterable,$WORD);

extern struct $Ord$bytes$class $Ord$bytes$methods;
$Ord$bytes $Ord$bytes$new();
extern struct $Hashable$bytes$class $Hashable$bytes$methods;
$Hashable$bytes $Hashable$bytes$new();
extern struct $Times$bytes$class $Times$bytes$methods;
$Times$bytes $Times$bytes$new();
extern struct $Sliceable$bytes$class $Sliceable$bytes$methods;
$Sliceable$bytes $Sliceable$bytes$new();
extern struct $Container$bytes$class $Container$bytes$methods;
$Container$bytes $Container$bytes$new();

extern struct $Ord$bytes *$Ord$bytes$witness;
extern struct $Hashable$bytes *$Hashable$bytes$witness;
extern struct $Times$bytes *$Times$bytes$witness;
extern struct $Sliceable$bytes *$Sliceable$bytes$witness;
extern struct $Container$bytes *$Container$bytes$witness;

$bytes to$bytes(char *str);
$bytes to$bytes_len(char *str, int len);
unsigned char *from$bytes($bytes b);


// Iterators over bytess ///////////////////////////////////////////////////////


typedef struct $Iterator$bytes *$Iterator$bytes; ;

struct $Iterator$bytes$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Iterator$bytes, $bytes);
    void (*__serialize__)($Iterator$bytes,$Serial$state);
    $Iterator$bytes (*__deserialize__)($Iterator$bytes,$Serial$state);
    $bool (*__bool__)($Iterator$bytes);
    $str (*__str__)($Iterator$bytes);
    $str (*__repr__)($Iterator$bytes);
    $int (*__next__)($Iterator$bytes);
};

struct $Iterator$bytes {
    struct $Iterator$bytes$class *$class;
    $bytes src;
    int nxt;
};

extern struct  $Iterator$bytes$class  $Iterator$bytes$methods;
$Iterator$bytes $Iterator$bytes$new($bytes);

//builtin functions //////////////////////////////////////////////////////////////////////////////////

// Backslash, single and double quote are always escaped.
// All control and non-ASCII bytes are escaped using \xhh (so \x0a is used instead of \n, etc)
// Single quotes are used as string delimiters.
$str $ascii($str s);
$str $bin($Integral wit, $WORD n);
$str $chr($Integral wit, $WORD n);
$str $hex($Integral wit, $WORD n);
$int $ord($str c);


// Internal auxiliary function /////////////////////////////////////////////

// used in defining __str__ method for collection types (list, dict, set)
$str $str_join_par(char lpar,$list elems, char rpar);

$str $default__str__($value);
