// Super //////////////////////////////////////////////////////////////

// This is only used for typing the $superclass field in classes.


struct $SuperG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
};

struct $Super {
    $SuperG_class $class;
};

/*
typedef struct $InitializableG_class *$InitializableG_class;

typedef struct $Initializable  *$Initializable;

struct $InitializableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                   // = NULL
    B_NoneType (*__init__)($Initializable);
};

struct $Initializable {
    struct $InitializableG_class *$class;
};

extern struct $InitializableG_class $InitializableG_methods;
$Initializable $InitializableG_new();
*/

// Serializable //////////////////////////////////////////////////////

struct $SerializableG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;                   // = InitializableG_methods
    B_NoneType (*__init__)($Serializable);
    void (*__serialize__)($Serializable, $Serial$state);
    $Serializable (*__deserialize__)($Serializable, $Serial$state);
};

struct $Serializable {
    struct $SerializableG_class *$class;
};

extern struct $SerializableG_class $SerializableG_methods;
$Serializable $SerializableG_new();
  
// B_BoolD_value ////////////////////////////////////////////////////////////////

/* struct B_BoolD_value; */
/* typedef struct B_BoolD_value *B_BoolD_value; */

/* struct B_BoolD_valueG_class; */
/* typedef struct B_BoolD_valueG_class *B_BoolD_valueG_class; */

/* extern struct B_BoolD_valueG_class B_BoolD_valueG_methods; */
/* B_BoolD_value B_BoolD_valueG_new(); */

// B_ShowD_value ////////////////////////////////////////////////////////////////

/* struct B_ShowD_value; */
/* typedef struct B_ShowD_value *B_ShowD_value; */

/* struct B_ShowD_valueG_class; */
/* typedef struct B_ShowD_valueG_class *B_ShowD_valueG_class; */

/* extern struct B_ShowD_valueG_class B_ShowD_valueG_methods; */
/* B_ShowD_value B_ShowD_valueG_new(); */
