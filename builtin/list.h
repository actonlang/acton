typedef struct $list$__methods__ {
  None (*__serialize__)($list, $WORD*, int, $dict, $ROWLISTHEADER);
  $list (*__deserialize__)($ROW*, $dict);
  $list(*copy)($list);
  //  $int (*sort)($list self, int (*cmp)($WORD,$WORD));
} *$list$__methods__;

$list$__methods__ $list_methods;

struct $list {
  char *GCINFO;
  $list$__methods__ __class__;
  $WORD *data;
  int length;
  int capacity;
};


Sequence$list Sequence$list_new();
Container$list Container$list_new(Eq); // equality is for elements

void printlist($list list); //for debugging; only for lists of ints
void $list_init($list lst);
