// Super //////////////////////////////////////////////////////////////

// This is only used for typing the $superclass field in classes.

struct $Super$class;
typedef struct $Super$class *$Super$class;

struct $Super;
typedef struct $Super *$Super;

struct $Super$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
};

struct $Super {
  $Super$class $class;
};


// Initializable //////////////////////////////////////////////////////

// For the moment (end of Nov, 2020), we have no use for Initializable and Serializable

typedef struct $Initializable$class *$Initializable$class;

typedef struct $Initializable  *$Initializable;

struct $Initializable$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;                   // = NULL
  void (*__init__)($Initializable);
};

struct $Initializable {
  struct $Initializable$class *$class;
};

extern struct $Initializable$class $Initializable$methods;
$Initializable $Initializable$new();

// Serializable //////////////////////////////////////////////////////

typedef struct $Serializable$class *$Serializable$class;

typedef struct $Serializable  *$Serializable;

struct $Serializable$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;                   // = Initializable$methods
  void (*__init__)($Serializable);
  void (*__serialize__)($Serializable, $Serial$state);
  $Serializable (*__deserialize__)($Serial$state);
};

struct $Serializable {
  struct $Serializable$class *$class;
};

extern struct $Serializable$class $Serializable$methods;
$Serializable $Serializable$new();

// struct //////////////////////////////////////////////////////

// All user defined classes inherit from struct, as well as
// int, float, complex, str, bytes, tuple, range, NoneType (and immutable versions of list, set and dict)
// For the moment, closures, iterators and exceptions also inherit from struct


typedef struct $struct$class *$struct$class;

typedef struct $struct  *$struct;

struct $struct$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;                      // = Serializable$methods
  void (*__init__)($struct);
  void (*__serialize__)($struct, $Serial$state);
  $struct (*__deserialize__)($Serial$state);
  $bool (*__bool__)($struct);
  $str (*__str__)($struct);
};

struct $struct {
  struct $struct$class *$class;
};

extern struct $struct$class $struct$methods;
$struct $struct$new();

// object //////////////////////////////////////////////////////

// All mutable user defined classes inherit from object, 
// as well as list, dict, set, bytearray

typedef struct $object$class *$object$class;

typedef struct $object  *$object;

struct $object$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;                      // = $struct$methods
  void (*__init__)($object);
  void (*__serialize__)($object, $Serial$state);
  $object (*__deserialize__)($Serial$state);
  $bool (*__bool__)($object);
  $str (*__str__)($object);
};

struct $object {
  struct $object$class *$class;
};

extern struct $object$class $object$methods;
$object $object$new();

  
