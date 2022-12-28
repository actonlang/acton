#pragma once

struct $proc;
struct $action;
struct $mut;
struct $pure;
struct $Cont;
struct $Msg;

typedef struct $proc *$proc;
typedef struct $action *$action;
typedef struct $mut *$mut;
typedef struct $pure *$pure;
typedef struct $Cont *$Cont;
typedef struct $Msg *$Msg;


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


struct $Cont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Cont);
    void (*__serialize__)($Cont, $Serial$state);
    $Cont (*__deserialize__)($Cont, $Serial$state);
    $bool (*__bool__)($Cont);
    $str (*__str__)($Cont);
    $str (*__repr__)($Cont);
    $R (*__call__)($Cont, $WORD);
};
struct $Cont {
    struct $Cont$class *$class;
};
extern struct $Cont$class $Cont$methods;

void $Cont$__init__($Cont);
$bool $Cont$__bool__($Cont);
$str $Cont$__str__($Cont);
void $Cont$__serialize__($Cont, $Serial$state);
$Cont $Cont$__deserialize__($Cont, $Serial$state);


struct $proc$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($proc);
    void (*__serialize__)($proc, $Serial$state);
    $proc (*__deserialize__)($proc, $Serial$state);
    $bool (*__bool__)($proc);
    $str (*__str__)($proc);
    $str (*__repr__)($proc);
    $R (*__call__)($proc, $Cont, $WORD);
    $R (*__exec__)($proc, $Cont, $WORD);
};
struct $proc {
    struct $proc$class *$class;
};
extern struct $proc$class $proc$methods;


struct $action$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($action);
    void (*__serialize__)($action, $Serial$state);
    $action (*__deserialize__)($action, $Serial$state);
    $bool (*__bool__)($action);
    $str (*__str__)($action);
    $str (*__repr__)($action);
    $R (*__call__)($action, $Cont, $WORD);
    $R (*__exec__)($action, $Cont, $WORD);
    $Msg (*__asyn__)($action, $WORD);
};
struct $action {
    struct $action$class *$class;
};
extern struct $action$class $action$methods;


struct $mut$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($mut);
    void (*__serialize__)($mut, $Serial$state);
    $mut (*__deserialize__)($mut, $Serial$state);
    $bool (*__bool__)($mut);
    $str (*__str__)($mut);
    $str (*__repr__)($mut);
    $R (*__call__)($mut, $Cont, $WORD);
    $R (*__exec__)($mut, $Cont, $WORD);
    $WORD (*__eval__)($mut, $WORD);
};
struct $mut {
    struct $mut$class *$class;
};
extern struct $mut$class $mut$methods;


struct $pure$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($pure);
    void (*__serialize__)($pure, $Serial$state);
    $pure (*__deserialize__)($pure, $Serial$state);
    $bool (*__bool__)($pure);
    $str (*__str__)($pure);
    $str (*__repr__)($pure);
    $R (*__call__)($pure, $Cont, $WORD);
    $R (*__exec__)($pure, $Cont, $WORD);
    $WORD (*__eval__)($pure, $WORD);
};
struct $pure {
    struct $pure$class *$class;
};
extern struct $pure$class $pure$methods;


//////////////////////////////////////////////////////////////////////////////////
struct $action2;
typedef struct $action2 *$action2;
struct $action2$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($action2);
    void (*__serialize__)($action2, $Serial$state);
    $action2 (*__deserialize__)($action2, $Serial$state);
    $bool (*__bool__)($action2);
    $str (*__str__)($action2);
    $str (*__repr__)($action2);
    $R (*__call__)($action2, $Cont, $WORD, $WORD);
    $R (*__exec__)($action2, $Cont, $WORD, $WORD);
    $Msg (*__asyn__)($action2, $WORD, $WORD);
};
struct $action2 {
    struct $action2$class *$class;
};

struct $action3;
typedef struct $action3 *$action3;
struct $action3$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($action3);
    void (*__serialize__)($action3, $Serial$state);
    $action3 (*__deserialize__)($action3, $Serial$state);
    $bool (*__bool__)($action3);
    $str (*__str__)($action3);
    $str (*__repr__)($action3);
    $R (*__call__)($action3, $Cont, $WORD, $WORD, $WORD);
    $R (*__exec__)($action3, $WORD, $WORD, $WORD);
    $Msg (*__asyn__)($action3, $WORD, $WORD, $WORD);
};
struct $action3 {
    struct $action3$class *$class;
};
