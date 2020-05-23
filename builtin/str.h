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
  void (*__init__)($str, char*);
  void (*__serialize__)($str,$Serial$state);
  $str (*__deserialize__)($Serial$state);
  $str (*capitalize)($str s);
  $str (*center)($str s, int width, $str fill);                 // raises TYPEERROR if fill is not a single char
  $int (*count)($str s, $str sub, $int start, $int end);
  //void encode($str s, bytes_t *res);                          // only utf-8 encoding and strict error handling
  $bool (*endswith)($str s, $str suffix, $int start, $int end);
  $str (*expandtabs)($str s, int tabsize);     
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
  $str (*join)($str sep, $Iterable$opaque it);
  $str (*ljust)($str s, int width, $str fill);                   // raises TYPEERROR if fill is not a single char
  $str (*lower)($str s);
  $str (*lstrip)($str s,$str cs);                                // cs may be NULL, then defaulting to whitespace removal.
  //maketrans not implemented
  void (*partition)($str s, $str sep, $str *ls, $str *ssep, $str *rs);
  $str (*replace)($str s, $str old, $str new, $int count);
  $int (*rfind)($str s, $str sub, $int start, $int end);         // returns -1 when not found
  $int (*rindex)($str s, $str sub, $int start, $int end);        // like rfind but raises VALUEERROR when not found
  $str (*rjust)($str s, int width, $str fill);                   // raises TYPEERROR if fill is not a single char
  void (*rpartition)($str s, $str sep, $str *ls, $str *ssep, $str *rs); 
  //int (*rsplit)($str s, $str sep, int maxsplit, list_t *res);  // not implemented sep may be NULL; then separation is indicated by a whitespace string
  $str (*rstrip)($str s,$str cs);                                //  cs may be NULL, then defaulting to whitespace removal.
  $list (*split)($str s, $str sep, $int maxsplit);               // raises VALUEERROR when separator is empty string
  $list (*splitlines)($str s);                                   // keepends parameter absent; only \n recognized as line separator
  $bool (*startswith)($str s, $str prefix, $int start, $int end); 
  $str (*strip)($str s,$str cs);                                 // cs may be NULL, then defaulting to whitespace removal.
// translate not implemented
  $str (*upper)($str s);
  $str (*$str_zfill)($str s, int width);
};

extern struct $int$class $int$methods;

extern struct $Ord$str$class $Ord$strmethods;
extern struct $Hashable$str$class $Hashable$strmethods;
extern struct $Plus$str$class $Plus$strmethods;
extern struct $Sliceable$str$class $Sliceable$strmethods;
extern struct $Container$str$class $Container$strmethods;

extern struct $Ord$str *$Ord$str$witness;
extern struct $Hashable$str *$Hashable$str$witness;
extern struct $Plus$str *$Plus$str$witness;
extern struct $Sliceable$str *$Sliceable$str$witness;
extern struct $Container$str *$Container$str$witness;



// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a $str value.
$str from$UTF8(char *str);
// Destructor; recover the internal string.
unsigned char *to$UTF8($str str);

// Iterators over strs ///////////////////////////////////////////////////////

typedef struct $Iterator$str *$Iterator$str; ;

struct $Iterator$str$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$str, $str);
  void (*__serialize__)($Iterator$str,$Serial$state);
  $Iterator$str (*__deserialize__)($Serial$state);
  $str (*__next__)($Iterator$str);
};

struct $Iterator$str {
  struct $Iterator$str$class *$class;
  $str src;
  int nxt;
};

extern struct  $Iterator$str$class  $Iterator$str$methods;
