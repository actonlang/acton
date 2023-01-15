#pragma once

struct $proc;
struct $action;
struct $mut;
struct $pure;
struct $Cont;
struct B_Msg;

typedef struct $proc *$proc;
typedef struct $action *$action;
typedef struct $mut *$mut;
typedef struct $pure *$pure;
typedef struct $Cont *$Cont;
typedef struct B_Msg *B_Msg;


enum $RTAG { $RDONE, $RFAIL, $RCONT, $RWAIT };
typedef enum $RTAG $RTAG;

struct $R {
    $RTAG tag;
    $Cont cont;
    $WORD value;
};
typedef struct $R $R;

#define $RU_CONT $R_CONT        // Temporary workaround until Prim names get their own prefix

#define $R_CONT(cont, arg)      ($R){$RCONT, (cont), ($WORD)(arg)}
#define $R_DONE(value)          ($R){$RDONE, NULL,   (value)}
#define $R_FAIL(value)          ($R){$RFAIL, NULL,   (value)}
#define $R_WAIT(cont, value)    ($R){$RWAIT, (cont), (value)}


struct $ContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Cont);
    void (*__serialize__)($Cont, $Serial$state);
    $Cont (*__deserialize__)($Cont, $Serial$state);
    B_bool (*__bool__)($Cont);
    B_str (*__str__)($Cont);
    B_str (*__repr__)($Cont);
    $R (*__call__)($Cont, $WORD);
};
struct $Cont {
    struct $ContG_class *$class;
};
extern struct $ContG_class $ContG_methods;

void $ContD___init__($Cont);
B_bool $ContD___bool__($Cont);
B_str $ContD___str__($Cont);
void $ContD___serialize__($Cont, $Serial$state);
$Cont $ContD___deserialize__($Cont, $Serial$state);


struct $procG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($proc);
    void (*__serialize__)($proc, $Serial$state);
    $proc (*__deserialize__)($proc, $Serial$state);
    B_bool (*__bool__)($proc);
    B_str (*__str__)($proc);
    B_str (*__repr__)($proc);
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
    B_bool (*__bool__)($action);
    B_str (*__str__)($action);
    B_str (*__repr__)($action);
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
    B_bool (*__bool__)($mut);
    B_str (*__str__)($mut);
    B_str (*__repr__)($mut);
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
    B_bool (*__bool__)($pure);
    B_str (*__str__)($pure);
    B_str (*__repr__)($pure);
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
    B_bool (*__bool__)($action2);
    B_str (*__str__)($action2);
    B_str (*__repr__)($action2);
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
    B_bool (*__bool__)($action3);
    B_str (*__str__)($action3);
    B_str (*__repr__)($action3);
    $R (*__call__)($action3, $Cont, $WORD, $WORD, $WORD);
    $R (*__exec__)($action3, $WORD, $WORD, $WORD);
    B_Msg (*__asyn__)($action3, $WORD, $WORD, $WORD);
};
struct $action3 {
    struct $action3G_class *$class;
};
