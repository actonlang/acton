// Super //////////////////////////////////////////////////////////////

// This is only used for typing the $superclass field in classes.

struct $SuperG_class;
typedef struct $SuperG_class *$SuperG_class;

struct $Super;
typedef struct $Super *$Super;

struct $SuperG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
};

struct $Super {
    $SuperG_class $class;
};


// Initializable //////////////////////////////////////////////////////

// For the moment (end of Nov, 2020), we have no use for Initializable and Serializable

typedef struct $InitializableG_class *$InitializableG_class;

typedef struct $Initializable  *$Initializable;

struct $InitializableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                   // = NULL
    void (*__init__)($Initializable);
};

struct $Initializable {
    struct $InitializableG_class *$class;
};

extern struct $InitializableG_class $InitializableG_methods;
$Initializable $InitializableG_new();

// Serializable //////////////////////////////////////////////////////

typedef struct $SerializableG_class *$SerializableG_class;

typedef struct $Serializable  *$Serializable;

struct $SerializableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                   // = InitializableG_methods
    void (*__init__)($Serializable);
    void (*__serialize__)($Serializable, $Serial$state);
    $Serializable (*__deserialize__)($Serializable, $Serial$state);
};

struct $Serializable {
    struct $SerializableG_class *$class;
};

extern struct $SerializableG_class $SerializableG_methods;
$Serializable $SerializableG_new();

// struct //////////////////////////////////////////////////////

// All user defined classes inherit from struct, as well as
// int, float, complex, str, bytes, tuple, range, B_NoneType (and immutable versions of list, set and dict)
// For the moment, closures, iterators and exceptions also inherit from struct


typedef struct B_valueG_class *B_valueG_class;

typedef struct B_value  *B_value;

struct B_valueG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                      // = SerializableG_methods
    void (*__init__)(B_value);
    void (*__serialize__)(B_value, $Serial$state);
    B_value (*__deserialize__)(B_value, $Serial$state);
    B_bool (*__bool__)(B_value);
    B_str (*__str__)(B_value);
    B_str (*__repr__)(B_value);
};

struct B_value {
    struct B_valueG_class *$class;
};

extern struct B_valueG_class B_valueG_methods;
B_value B_valueG_new();

// object //////////////////////////////////////////////////////

// All mutable user defined classes inherit from object, 
// as well as list, dict, set, bytearray

typedef struct B_objectG_class *B_objectG_class;

typedef struct B_object  *B_object;

struct B_objectG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                      // = B_valueG_methods
    void (*__init__)(B_object);
    void (*__serialize__)(B_object, $Serial$state);
    B_object (*__deserialize__)(B_object, $Serial$state);
    B_bool (*__bool__)(B_object);
    B_str (*__str__)(B_object);
    B_str (*__repr__)(B_object);
};

struct B_object {
    struct B_objectG_class *$class;
};

extern struct B_objectG_class B_objectG_methods;
B_object B_objectG_new();

  
