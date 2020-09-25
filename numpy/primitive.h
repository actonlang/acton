// The bulk of data in an ndarray is stored in a C array of union $Bytes8 data.
// Each ndarray also holds the address of an $UnboxedFunctions struct, containing conversion
// functions to and from boxed data, and operators on unboxed data.
// This file provides the necessary type definitions and $UnboxedFunctions structs for the
// two Acton types supported at the moment, int and float (i.e., boxed long and double).

struct $Primitive;
typedef struct $Primitive *$Primitive;

struct $Primitive$class;
typedef struct $Primitive$class *$Primitive$class;

struct $Primitive$int;
typedef struct $Primitive$int *$Primitive$int;

struct $Primitive$int$class;
typedef struct $Primitive$int$class *$Primitive$int$class;

struct $Primitive$float;
typedef struct $Primitive$float *$Primitive$float;

struct $Primitive$float$class;
typedef struct $Primitive$float$class *$Primitive$float$class;

struct $Primitive {
    $Primitive$class $class;
};

union $Bytes8 {
  long l;
  double d;
};

enum ElemType {LongType,DblType};

int elem_size(enum ElemType typ);

struct $Primitive$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Primitive);
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
  union $Bytes8 (*band)(union $Bytes8, union $Bytes8);
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

struct $Primitive$int {
    $Primitive$int$class $class;
};

struct $Primitive$int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Primitive$int);
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

struct $Primitive$float {
  $Primitive$float$class $class;
};

struct $Primitive$float$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Primitive$float);
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

// Witnesses ////////////////////////////////////////////////////////////////////////////

extern struct $Primitive$int$class  $Primitive$int$methods;
extern struct $Primitive$float$class  $Primitive$float$methods;

extern struct $Primitive$int *$Primitive$int$witness;
extern struct $Primitive$float *$Primitive$float$witness;



