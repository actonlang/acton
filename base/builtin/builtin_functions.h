#pragma once
// #include "function.h"

 
// enumerate ////////////////////////////////////////////////////////////

struct B_IteratorD_enumerate;
typedef struct B_IteratorD_enumerate *B_IteratorD_enumerate;

struct B_IteratorD_enumerateG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_enumerate, B_Iterator,B_int);
  void (*__serialize__)(B_IteratorD_enumerate,$Serial$state);
  B_IteratorD_enumerate (*__deserialize__)(B_IteratorD_enumerate,$Serial$state);
  B_bool (*__bool__)(B_IteratorD_enumerate);
  B_str (*__str__)(B_IteratorD_enumerate);
  B_str (*__repr__)(B_IteratorD_enumerate);
  $WORD(*__next__)(B_IteratorD_enumerate);
};

struct B_IteratorD_enumerate {
  struct B_IteratorD_enumerateG_class *$class;
  B_Iterator it;
  int nxt;
};

extern struct B_IteratorD_enumerateG_class B_IteratorD_enumerateG_methods;
B_IteratorD_enumerate B_IteratorD_enumerateG_new(B_Iterator,B_int);


// filter ////////////////////////////////////////////////////////////

B_Iterator B_filter (B_Iterable, $pure, $WORD);

struct B_IteratorD_filter;
typedef struct B_IteratorD_filter *B_IteratorD_filter;

struct B_IteratorD_filterG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_filter, B_Iterator, $pure);
  void (*__serialize__)(B_IteratorD_filter,$Serial$state);
  B_IteratorD_filter (*__deserialize__)(B_IteratorD_filter,$Serial$state);
  B_bool (*__bool__)(B_IteratorD_filter);
  B_str (*__str__)(B_IteratorD_filter);
  B_str (*__repr__)(B_IteratorD_filter);
  $WORD(*__next__)(B_IteratorD_filter);
};

struct B_IteratorD_filter {
  struct B_IteratorD_filterG_class *$class;
  B_Iterator it;
  $pure f;
};

extern struct B_IteratorD_filterG_class B_IteratorD_filterG_methods;
B_IteratorD_filter B_IteratorD_filterG_new(B_Iterator, $pure);

// map ////////////////////////////////////////////////////////////

B_Iterator B_map (B_Iterable, $pure, $WORD);

struct B_IteratorD_map;
typedef struct B_IteratorD_map *B_IteratorD_map;

struct B_IteratorD_mapG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_map, B_Iterator, $pure);
  void (*__serialize__)(B_IteratorD_map,$Serial$state);
  B_IteratorD_map (*__deserialize__)(B_IteratorD_map,$Serial$state);
  B_bool (*__bool__)(B_IteratorD_map);
  B_str (*__str__)(B_IteratorD_map);
  B_str (*__repr__)(B_IteratorD_map);
  $WORD(*__next__)(B_IteratorD_map);
};

struct B_IteratorD_map {
  struct B_IteratorD_mapG_class *$class;
  B_Iterator it;
  $pure f;
};

extern struct B_IteratorD_mapG_class B_IteratorD_mapG_methods;
B_IteratorD_map B_IteratorD_mapG_new(B_Iterator, $pure);

// zip ////////////////////////////////////////////////////////////

struct B_IteratorD_zip;
typedef struct B_IteratorD_zip *B_IteratorD_zip;

struct B_IteratorD_zipG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(B_IteratorD_zip, B_Iterator, B_Iterator);
  void (*__serialize__)(B_IteratorD_zip,$Serial$state);
  B_IteratorD_zip (*__deserialize__)(B_IteratorD_zip,$Serial$state);
  B_bool (*__bool__)(B_IteratorD_zip);
  B_str (*__str__)(B_IteratorD_zip);
  B_str (*__repr__)(B_IteratorD_zip);
  $WORD(*__next__)(B_IteratorD_zip);
};

struct B_IteratorD_zip {
  struct B_IteratorD_zipG_class *$class;
  B_Iterator it1;
  B_Iterator it2;
};

extern struct B_IteratorD_zipG_class B_IteratorD_zipG_methods;
B_IteratorD_zip B_IteratorD_zipG_new(B_Iterator, B_Iterator);

B_Iterator B_zip(B_Iterable wit1, B_Iterable wit2, $WORD iter1, $WORD iter2);


// EqOpt //////////////////////////////////////////////////////

struct $EqOpt;
typedef struct $EqOpt *$EqOpt;

struct $EqOptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($EqOpt, B_Eq);
    void (*__serialize__)($EqOpt,$Serial$state);
    $EqOpt (*__deserialize__)($EqOpt,$Serial$state);
    B_bool (*__bool__)($EqOpt);
    B_str (*__str__)($EqOpt);
    B_str (*__repr__)($EqOpt);
    B_bool (*__eq__)($EqOpt, $WORD, $WORD);
    B_bool (*__ne__)($EqOpt, $WORD, $WORD);
};

struct $EqOpt {
    struct $EqOptG_class *$class;
    B_Eq W_Eq$A;
};

$EqOpt $EqOptG_new(B_Eq);


// IdentityActor //////////////////////////////////////////////////////

struct $IdentityActor;
typedef struct $IdentityActor *$IdentityActor;

struct $IdentityActorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($IdentityActor);
    void (*__serialize__)($IdentityActor,$Serial$state);
    $IdentityActor (*__deserialize__)($IdentityActor,$Serial$state);
    B_bool (*__bool__)($IdentityActor);
    B_str (*__str__)($IdentityActor);
    B_str (*__repr__)($IdentityActor);
    B_bool (*__is__)($IdentityActor, $WORD, $WORD);
    B_bool (*__isnot__)($IdentityActor, $WORD, $WORD);
};

struct $IdentityActor {
    struct $IdentityActorG_class *$class;
};

$IdentityActor $IdentityActorG_new();


// Various small functions //////////////////////////////////////////////////////////

$WORD B_min(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD deflt);
$WORD B_max(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD deflt);
$WORD B_max_def(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD deflt);
$WORD B_min_def(B_Ord wit, B_Iterable wit2, $WORD iter, $WORD deflt);

// Signatures generated by actonc 

$WORD B_abs (B_Real, B_Number, $WORD);
/*
B_bool B_all (B_Iterable, $WORD);
B_bool B_any (B_Iterable, $WORD);
B_tuple B_divmod (B_Integral, $WORD, $WORD);
B_int B_hash (B_Hashable, $WORD);
B_Iterator B_iter (B_Iterable, $WORD);
B_int B_len (B_Collection, $WORD);
$WORD $next (B_Iterator);
$WORD B_pow (B_Number, $WORD, $WORD);
B_str B_repr(B_value);
B_Iterator B_reversed (B_Sequence, $WORD);
$WORD B_round (B_Real, $WORD, B_int);
$WORD B_sum(B_Plus, B_Iterable, $WORD, $WORD);
*/
