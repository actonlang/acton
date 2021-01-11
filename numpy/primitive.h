#pragma once

// The bulk of data in an ndarray is stored in a C array of union $Bytes8 data.
// Each ndarray also holds the address of an $UnboxedFunctions struct, containing conversion
// functions to and from boxed data, and operators on unboxed data.
// This file provides the necessary type definitions and $UnboxedFunctions structs for the
// two Acton types supported at the moment, int and float (i.e., boxed long and double).



struct numpy$$Primitive;
typedef struct numpy$$Primitive *numpy$$Primitive;

struct numpy$$Primitive$class;
typedef struct numpy$$Primitive$class *numpy$$Primitive$class;

struct numpy$$Primitive$int;
typedef struct numpy$$Primitive$int *numpy$$Primitive$int;

struct numpy$$Primitive$int$class;
typedef struct numpy$$Primitive$int$class *numpy$$Primitive$int$class;

struct numpy$$Primitive$float;
typedef struct numpy$$Primitive$float *numpy$$Primitive$float;

struct numpy$$Primitive$float$class;
typedef struct numpy$$Primitive$float$class *numpy$$Primitive$float$class;

struct numpy$$Primitive {
    numpy$$Primitive$class $class;
};

union $Bytes8 {
  long l;
  double d;
};

enum ElemType {LongType,DblType};

int $elem_size(enum ElemType typ);

struct numpy$$Primitive$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive);
  void (*__serialize__)(numpy$$Primitive,$Serial$state); 
  numpy$$Primitive (*__deserialize__)(numpy$$Primitive,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive);
  $str (*__str__)(numpy$$Primitive);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);                      
  union $Bytes8 (*from$obj)($WORD);
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$div)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);        
  void (*$isub)(union $Bytes8*, union $Bytes8);        
  void (*$imul)(union $Bytes8*, union $Bytes8);        
  void (*$idiv)(union $Bytes8*, union $Bytes8);        
  void (*$imod)(union $Bytes8*, union $Bytes8);       
  void (*$iband)(union $Bytes8*, union $Bytes8);      
  void (*$ibor)(union $Bytes8*, union $Bytes8);       
  void (*$ibxor)(union $Bytes8*, union $Bytes8);      
  void (*$ilsh)(union $Bytes8*, union $Bytes8);       
  void (*$irsh)(union $Bytes8*, union $Bytes8);       
  bool (*$eq)(union $Bytes8, union $Bytes8);   
  bool (*$neq)(union $Bytes8, union $Bytes8);  
  bool (*$lt)(union $Bytes8, union $Bytes8); 
  bool (*$le)(union $Bytes8, union $Bytes8); 
  bool (*$gt)(union $Bytes8, union $Bytes8); 
  bool (*$ge)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$abs)(union $Bytes8);                
  union $Bytes8 (*$neg)(union $Bytes8);                
  union $Bytes8 (*$lnot)(union $Bytes8);              
  union $Bytes8 (*$bnot)(union $Bytes8);              
};

$str l$prim_str(union $Bytes8 n);
$str d$prim_str(union $Bytes8 n);

// Primitive instance for int ///////////////////////////////////////////////////////////////

struct numpy$$Primitive$int {
    numpy$$Primitive$int$class $class;
};

struct numpy$$Primitive$int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive$int);
  void (*__serialize__)(numpy$$Primitive$int,$Serial$state); 
  numpy$$Primitive$int (*__deserialize__)(numpy$$Primitive$int,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive$int);
  $str (*__str__)(numpy$$Primitive$int);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);                      
  union $Bytes8 (*from$obj)($WORD);                    
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$div)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);        
  void (*$isub)(union $Bytes8*, union $Bytes8);        
  void (*$imul)(union $Bytes8*, union $Bytes8);        
  void (*$idiv)(union $Bytes8*, union $Bytes8);        
  void (*$imod)(union $Bytes8*, union $Bytes8);       
  void (*$iband)(union $Bytes8*, union $Bytes8);      
  void (*$ibor)(union $Bytes8*, union $Bytes8);       
  void (*$ibxor)(union $Bytes8*, union $Bytes8);      
  void (*$ilsh)(union $Bytes8*, union $Bytes8);       
  void (*$irsh)(union $Bytes8*, union $Bytes8);       
  bool (*$eq)(union $Bytes8, union $Bytes8);   
  bool (*$neq)(union $Bytes8, union $Bytes8);  
  bool (*$lt)(union $Bytes8, union $Bytes8); 
  bool (*$le)(union $Bytes8, union $Bytes8); 
  bool (*$gt)(union $Bytes8, union $Bytes8); 
  bool (*$ge)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$abs)(union $Bytes8);                
  union $Bytes8 (*$neg)(union $Bytes8);                
  union $Bytes8 (*$lnot)(union $Bytes8);              
  union $Bytes8 (*$bnot)(union $Bytes8);              
};

// Primitive instance for float ///////////////////////////////////////////////////////////////

struct numpy$$Primitive$float {
  numpy$$Primitive$float$class $class;
};

struct numpy$$Primitive$float$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive$float);
  void (*__serialize__)(numpy$$Primitive$float,$Serial$state); 
  numpy$$Primitive$float (*__deserialize__)(numpy$$Primitive$float,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive$float);
  $str (*__str__)(numpy$$Primitive$float);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);                      
  union $Bytes8 (*from$obj)($WORD);
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$div)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);        
  void (*$isub)(union $Bytes8*, union $Bytes8);        
  void (*$imul)(union $Bytes8*, union $Bytes8);        
  void (*$idiv)(union $Bytes8*, union $Bytes8);        
  void (*$imod)(union $Bytes8*, union $Bytes8);       
  void (*$iband)(union $Bytes8*, union $Bytes8);      
  void (*$ibor)(union $Bytes8*, union $Bytes8);       
  void (*$ibxor)(union $Bytes8*, union $Bytes8);      
  void (*$ilsh)(union $Bytes8*, union $Bytes8);       
  void (*$irsh)(union $Bytes8*, union $Bytes8);       
  bool (*$eq)(union $Bytes8, union $Bytes8);   
  bool (*$neq)(union $Bytes8, union $Bytes8);  
  bool (*$lt)(union $Bytes8, union $Bytes8); 
  bool (*$le)(union $Bytes8, union $Bytes8); 
  bool (*$gt)(union $Bytes8, union $Bytes8); 
  bool (*$ge)(union $Bytes8, union $Bytes8); 
  union $Bytes8 (*$abs)(union $Bytes8);                
  union $Bytes8 (*$neg)(union $Bytes8);                
  union $Bytes8 (*$lnot)(union $Bytes8);              
  union $Bytes8 (*$bnot)(union $Bytes8);              
};

// Witnesses and creation ////////////////////////////////////////////////////////////////////////////

numpy$$Primitive$int numpy$$Primitive$int$new();
numpy$$Primitive$float numpy$$Primitive$float$new();

extern struct numpy$$Primitive$int$class  numpy$$Primitive$int$methods;
extern struct numpy$$Primitive$float$class  numpy$$Primitive$float$methods;

extern struct numpy$$Primitive$int *numpy$$Primitive$int$witness;
extern struct numpy$$Primitive$float *numpy$$Primitive$float$witness;



