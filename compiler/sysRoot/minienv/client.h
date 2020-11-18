#ifndef client
#define client
#include "builtin.h"
#include "rts.h"
#include "minienv.h"
struct client$$l$1lambda;
struct client$$l$2lambda;
struct client$$l$4lambda;
struct client$$l$6lambda;
struct client$$l$8lambda;
struct client$$l$9lambda;
struct client$$client;
typedef struct client$$l$1lambda *client$$l$1lambda;
typedef struct client$$l$2lambda *client$$l$2lambda;
typedef struct client$$l$4lambda *client$$l$4lambda;
typedef struct client$$l$6lambda *client$$l$6lambda;
typedef struct client$$l$8lambda *client$$l$8lambda;
typedef struct client$$l$9lambda *client$$l$9lambda;
typedef struct client$$client *client$$client;
struct client$$l$1lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$1lambda, minienv$$Connection, client$$client);
    $NoneType (*__serialize__) (client$$l$1lambda, $Serial$state);
    client$$l$1lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$1lambda);
    $bool (*__bool__) (client$$l$1lambda);
    $R (*__call__) (client$$l$1lambda, $Cont);
};
struct client$$l$1lambda {
    struct client$$l$1lambda$class *$class;
    minienv$$Connection t$y;
    client$$client __self__;
};
struct client$$l$2lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$2lambda, client$$client);
    $NoneType (*__serialize__) (client$$l$2lambda, $Serial$state);
    client$$l$2lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$2lambda);
    $bool (*__bool__) (client$$l$2lambda);
    $Msg (*__call__) (client$$l$2lambda, minienv$$Connection);
};
struct client$$l$2lambda {
    struct client$$l$2lambda$class *$class;
    client$$client __self__;
};
struct client$$l$4lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$4lambda, minienv$$Env);
    $NoneType (*__serialize__) (client$$l$4lambda, $Serial$state);
    client$$l$4lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$4lambda);
    $bool (*__bool__) (client$$l$4lambda);
    $NoneType (*__call__) (client$$l$4lambda, $str);
};
struct client$$l$4lambda {
    struct client$$l$4lambda$class *$class;
    minienv$$Env l$3self;
};
struct client$$l$6lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$6lambda, minienv$$Env);
    $NoneType (*__serialize__) (client$$l$6lambda, $Serial$state);
    client$$l$6lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$6lambda);
    $bool (*__bool__) (client$$l$6lambda);
    $NoneType (*__call__) (client$$l$6lambda, $str);
};
struct client$$l$6lambda {
    struct client$$l$6lambda$class *$class;
    minienv$$Env l$5self;
};
struct client$$l$8lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$8lambda, minienv$$Connection);
    $NoneType (*__serialize__) (client$$l$8lambda, $Serial$state);
    client$$l$8lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$8lambda);
    $bool (*__bool__) (client$$l$8lambda);
    $NoneType (*__call__) (client$$l$8lambda, $str);
};
struct client$$l$8lambda {
    struct client$$l$8lambda$class *$class;
    minienv$$Connection l$7self;
};
struct client$$l$9lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (client$$l$9lambda, client$$client, minienv$$Connection);
    $NoneType (*__serialize__) (client$$l$9lambda, $Serial$state);
    client$$l$9lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$9lambda);
    $bool (*__bool__) (client$$l$9lambda);
    $R (*__call__) (client$$l$9lambda, $Cont);
};
struct client$$l$9lambda {
    struct client$$l$9lambda$class *$class;
    client$$client __self__;
    minienv$$Connection conn;
};
struct client$$client$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $R (*__init__) (client$$client, minienv$$Env, $Cont);
    $NoneType (*__serialize__) (client$$client, $Serial$state);
    client$$client (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$client);
    $bool (*__bool__) (client$$client);
    $R (*session$local) (client$$client, minienv$$Connection, $Cont);
    $Msg (*session) (client$$client, minienv$$Connection);
};
struct client$$client {
    struct client$$client$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    $Lock $msg_lock;
    minienv$$Env env;
};
extern struct client$$l$1lambda$class client$$l$1lambda$methods;
client$$l$1lambda client$$l$1lambda$new(minienv$$Connection, client$$client);
extern struct client$$l$2lambda$class client$$l$2lambda$methods;
client$$l$2lambda client$$l$2lambda$new(client$$client);
extern struct client$$l$4lambda$class client$$l$4lambda$methods;
client$$l$4lambda client$$l$4lambda$new(minienv$$Env);
extern struct client$$l$6lambda$class client$$l$6lambda$methods;
client$$l$6lambda client$$l$6lambda$new(minienv$$Env);
extern struct client$$l$8lambda$class client$$l$8lambda$methods;
client$$l$8lambda client$$l$8lambda$new(minienv$$Connection);
extern struct client$$l$9lambda$class client$$l$9lambda$methods;
client$$l$9lambda client$$l$9lambda$new(client$$client, minienv$$Connection);
extern struct client$$client$class client$$client$methods;
$R client$$client$new(minienv$$Env, $Cont);
void client$$__init__ ();
#endif