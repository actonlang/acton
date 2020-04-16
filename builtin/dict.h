typedef struct $dict$__methods__ {
  None(*__serialize__)($dict, $WORD*, int, $dict, $ROWLISTHEADER);
  $dict (*__deserialize__)($ROW*, $dict);
  Hashable (*__hashwitness__)($dict);
} *$dict$__methods__;

typedef struct $table_struct *$table;

struct $dict {
  char *$GCINFO;
  $dict$__methods__ __class__;
  long numelements;               // nr of elements in dictionary
  Hashable hashwit;
  $table table;                   // the hashtable
};

None $dict_serialize($dict, $WORD*, int, $dict, $ROWLISTHEADER);
$dict $dict_deserialize($ROW*, $dict);


$dict$__methods__ $dict_methods;

Mapping$dict Mapping$dict_new(Hashable);
