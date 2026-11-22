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
  

// Stateless protocol entry points share the native, witness-free workers.
B_str B_valueD___str__(B_value);
B_str B_valueD___repr__(B_value);
B_str B_objectD___str__(B_object);
bool B_valueD___bool__(B_value);
bool B_objectD___bool__(B_object);
#define B_DEFINE_SHOW(T) \
    B_str B_ShowD_##T##D___str__(B_ShowD_##T witness, B_##T self) { \
        return B_##T##_str(self); \
    } \
    B_str B_ShowD_##T##D___repr__(B_ShowD_##T witness, B_##T self) { \
        return B_##T##_repr(self); \
    }

#define B_DEFINE_BOOL(T) \
    bool B_BoolD_##T##D___bool__(B_BoolD_##T witness, B_##T self) { \
        return B_##T##_bool(self); \
    }
