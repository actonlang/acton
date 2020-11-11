#ifndef client
#define client
#include "minienv.h"
struct client$$l$1lambda;
struct client$$l$2lambda;
struct client$$l$5lambda;
struct client$$l$7lambda;
struct client$$l$9lambda;
struct client$$l$10lambda;
struct client$$l$11lambda;
struct client$$client;
typedef struct client$$l$1lambda *client$$l$1lambda;
typedef struct client$$l$2lambda *client$$l$2lambda;
typedef struct client$$l$5lambda *client$$l$5lambda;
typedef struct client$$l$7lambda *client$$l$7lambda;
typedef struct client$$l$9lambda *client$$l$9lambda;
typedef struct client$$l$10lambda *client$$l$10lambda;
typedef struct client$$l$11lambda *client$$l$11lambda;
typedef struct client$$client *client$$client;
struct client$$l$1lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$1lambda, minienv$$Connection, client$$client);
    void* (*__serialize__) (client$$l$1lambda, $Serial$state);
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
    void* (*__init__) (client$$l$2lambda, client$$client);
    void* (*__serialize__) (client$$l$2lambda, $Serial$state);
    client$$l$2lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$2lambda); 
    $bool (*__bool__) (client$$l$2lambda); 
    $Msg (*__call__) (client$$l$2lambda, minienv$$Connection); 
};
struct client$$l$2lambda {
    struct client$$l$2lambda$class *$class;
    client$$client __self__;
};
struct client$$l$5lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$5lambda, minienv$$Connection);
    void* (*__serialize__) (client$$l$5lambda, $Serial$state);
    client$$l$5lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$5lambda); 
    $bool (*__bool__) (client$$l$5lambda); 
    $R (*__call__) (client$$l$5lambda, $str, $Cont);
};
struct client$$l$5lambda {
    struct client$$l$5lambda$class *$class;
    minienv$$Connection l$4self;
};
$R client$$l$3c$1cont (client$$client, minienv$$Connection, $Cont, $Msg);
struct client$$l$7lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$7lambda, minienv$$Env);
    void* (*__serialize__) (client$$l$7lambda, $Serial$state);
    client$$l$7lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$7lambda); 
    $bool (*__bool__) (client$$l$7lambda); 
    $R (*__call__) (client$$l$7lambda, $str, $Cont);
};
struct client$$l$7lambda {
    struct client$$l$7lambda$class *$class;
    minienv$$Env l$6self;
};
struct client$$l$9lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$9lambda, minienv$$Env);
    void* (*__serialize__) (client$$l$9lambda, $Serial$state);
    client$$l$9lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$9lambda); 
    $bool (*__bool__) (client$$l$9lambda); 
    $R (*__call__) (client$$l$9lambda, $str, $Cont);
};
struct client$$l$9lambda {
    struct client$$l$9lambda$class *$class;
    minienv$$Env l$8self;
};
struct client$$l$10lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$10lambda, client$$client, minienv$$Connection, $Cont);
    void* (*__serialize__) (client$$l$10lambda, $Serial$state);
    client$$l$10lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$10lambda); 
    $bool (*__bool__) (client$$l$10lambda); 
    $R (*__call__) (client$$l$10lambda, $Msg);
};
struct client$$l$10lambda {
    struct client$$l$10lambda$class *$class;
    client$$client __self__;
    minienv$$Connection conn;
    $Cont c$cont;
};
struct client$$l$11lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (client$$l$11lambda, client$$client, minienv$$Connection);
    void* (*__serialize__) (client$$l$11lambda, $Serial$state);
    client$$l$11lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$l$11lambda); 
    $bool (*__bool__) (client$$l$11lambda); 
    $R (*__call__) (client$$l$11lambda, $Cont);
};
struct client$$l$11lambda {
    struct client$$l$11lambda$class *$class;
    client$$client __self__;
    minienv$$Connection conn;
};
struct client$$client$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $R (*__init__) (client$$client, minienv$$Env, $Cont);
    void* (*__serialize__) (client$$client, $Serial$state);
    client$$client (*__deserialize__) ($Serial$state);
    $str (*__str__) (client$$client); // abstract!!!
    $bool (*__bool__) (client$$client); // abstract!!!
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
extern struct client$$l$2lambda$class client$$l$2lambda$methods;
extern struct client$$l$5lambda$class client$$l$5lambda$methods;
extern struct client$$l$7lambda$class client$$l$7lambda$methods;
extern struct client$$l$9lambda$class client$$l$9lambda$methods;
extern struct client$$l$10lambda$class client$$l$10lambda$methods;
extern struct client$$l$11lambda$class client$$l$11lambda$methods;
extern struct client$$client$class client$$client$methods;

void client$$__init__();
#endif
