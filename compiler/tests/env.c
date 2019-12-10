#include "rts.h"

struct Env;
typedef struct Env *Env;
struct Env$__class__;
typedef struct Env$__class__ *Env$__class__;

struct Connection;
typedef struct Connection *Connection;
struct Connection$__class__;
typedef struct Connection$__class__ *Connection$__class__;

struct Env {
    Env$__class__ __class__;
    $WORD __impl__;
};
struct Connection {
    Connection$__class__ __class__;
    $WORD __impl__;
};
struct Env$__class__ {
    char *$GCINFO;
    $Msg (*open)(Env$__class__, $WORD, $str, $int, $Clos);
};
struct Connection$__class__ {
    char *$GCINFO;
    $Msg (*deliver)(Connection$__class__, $WORD, $str);
    $Msg (*close)(Connection$__class__, $WORD);
    $Msg (*receive_on)(Connection$__class__, $WORD, $Clos, $Clos);
};
