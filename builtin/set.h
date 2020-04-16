typedef struct $set$__methods__ {
  None (*__serialize__)($set, $WORD*, int, $dict, $ROWLISTHEADER);
  $set (*__deserialize__)($ROW*, $dict);
  Hashable (*__hashwitness__)($set);
  $set(*copy)($set);
} *$set$__methods__;

$set$__methods__ $set_methods;

None $set_serialize($set, $WORD*, int, $dict, $ROWLISTHEADER);
$set $set_deserialize($ROW*, $dict);


Set$set Set$set_new(Hashable);
