#pragma once

struct $ContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Cont);
    void (*__serialize__)($Cont, $Serial$state);
    $Cont (*__deserialize__)($Cont, $Serial$state);
    $R (*__call__)($Cont, $WORD);
};
struct $Cont {
    struct $ContG_class *$class;
};
extern struct $ContG_class $ContG_methods;

void $ContD___init__($Cont);
// B_bool $ContD___bool__($Cont);
// B_str $ContD___str__($Cont);
void $ContD___serialize__($Cont, $Serial$state);
$Cont $ContD___deserialize__($Cont, $Serial$state);


struct $procG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($proc);
    void (*__serialize__)($proc, $Serial$state);
    $proc (*__deserialize__)($proc, $Serial$state);
    $R (*__call__)($proc, $Cont, $WORD);
    $R (*__exec__)($proc, $Cont, $WORD);
};
struct $proc {
    struct $procG_class *$class;
};
extern struct $procG_class $procG_methods;


struct $actionG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    $R (*__call__)($action, $Cont, $WORD);
    $R (*__exec__)($action, $Cont, $WORD);
    B_Msg (*__asyn__)($action, $WORD);
};
struct $action {
    struct $actionG_class *$class;
};
extern struct $actionG_class $actionG_methods;


struct $mutG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($mut);
    void (*__serialize__)($mut, $Serial$state);
    $mut (*__deserialize__)($mut, $Serial$state);
    $R (*__call__)($mut, $Cont, $WORD);
    $R (*__exec__)($mut, $Cont, $WORD);
    $WORD (*__eval__)($mut, $WORD);
};
struct $mut {
    struct $mutG_class *$class;
};
extern struct $mutG_class $mutG_methods;


struct $pureG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($pure);
    void (*__serialize__)($pure, $Serial$state);
    $pure (*__deserialize__)($pure, $Serial$state);
    $R (*__call__)($pure, $Cont, $WORD);
    $R (*__exec__)($pure, $Cont, $WORD);
    $WORD (*__eval__)($pure, $WORD);
};
struct $pure {
    struct $pureG_class *$class;
};
extern struct $pureG_class $pureG_methods;


//////////////////////////////////////////////////////////////////////////////////
struct $action2;
typedef struct $action2 *$action2;
struct $action2G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action2);
    void (*__serialize__)($action2, $Serial$state);
    $action2 (*__deserialize__)($action2, $Serial$state);
    $R (*__call__)($action2, $Cont, $WORD, $WORD);
    $R (*__exec__)($action2, $Cont, $WORD, $WORD);
    B_Msg (*__asyn__)($action2, $WORD, $WORD);
};
struct $action2 {
    struct $action2G_class *$class;
};

struct $action3;
typedef struct $action3 *$action3;
struct $action3G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($action3);
    void (*__serialize__)($action3, $Serial$state);
    $action3 (*__deserialize__)($action3, $Serial$state);
    $R (*__call__)($action3, $Cont, $WORD, $WORD, $WORD);
    $R (*__exec__)($action3, $WORD, $WORD, $WORD);
    B_Msg (*__asyn__)($action3, $WORD, $WORD, $WORD);
};
struct $action3 {
    struct $action3G_class *$class;
};
