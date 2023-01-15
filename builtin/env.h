#pragma once
//////////////////////////////////////////////////////////////////////////////////
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <fcntl.h>
#include <strings.h>
#include <unistd.h>
#include <sys/errno.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>

#include "builtin.h"
#include "../rts/rts.h"

#ifdef IS_MACOS
#include <sys/event.h>
typedef struct kevent EVENT_type;
#endif

#ifdef IS_GNU_LINUX
#include <sys/epoll.h>
typedef struct epoll_event EVENT_type;
#endif

#define BUF_SIZE 1024

#define MAX_FD  1024

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;


///////////////////////////////////////////////////////////////////////////////////////////
// START GENERATED __builtin__.act
struct $l$1cont;
struct $l$2cont;
struct $l$3cont;
struct B_WorldAuth;
struct B_Env;
typedef struct $l$1cont *$l$1cont;
typedef struct $l$2cont *$l$2cont;
typedef struct $l$3cont *$l$3cont;
typedef struct B_WorldAuth *B_WorldAuth;
typedef struct B_Env *B_Env;
struct $l$1contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$1cont, B_Env, B_str);
    void (*__serialize__) ($l$1cont, $Serial$state);
    $l$1cont (*__deserialize__) ($l$1cont, $Serial$state);
    B_bool (*__bool__) ($l$1cont);
    B_str (*__str__) ($l$1cont);
    B_str (*__repr__) ($l$1cont);
    $R (*__call__) ($l$1cont, $Cont);
};
struct $l$1cont {
    struct $l$1contG_class *$class;
    B_Env self;
    B_str s;
};
struct $l$2contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$2cont, B_Env, $action);
    void (*__serialize__) ($l$2cont, $Serial$state);
    $l$2cont (*__deserialize__) ($l$2cont, $Serial$state);
    B_bool (*__bool__) ($l$2cont);
    B_str (*__str__) ($l$2cont);
    B_str (*__repr__) ($l$2cont);
    $R (*__call__) ($l$2cont, $Cont);
};
struct $l$2cont {
    struct $l$2contG_class *$class;
    B_Env self;
    $action cb;
};
struct $l$3contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$3cont, B_Env, B_int);
    void (*__serialize__) ($l$3cont, $Serial$state);
    $l$3cont (*__deserialize__) ($l$3cont, $Serial$state);
    B_bool (*__bool__) ($l$3cont);
    B_str (*__str__) ($l$3cont);
    B_str (*__repr__) ($l$3cont);
    $R (*__call__) ($l$3cont, $Cont);
};
struct $l$3cont {
    struct $l$3contG_class *$class;
    B_Env self;
    B_int n;
};
struct B_WorldAuthG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) (B_WorldAuth);
    void (*__serialize__) (B_WorldAuth, $Serial$state);
    B_WorldAuth (*__deserialize__) (B_WorldAuth, $Serial$state);
    B_bool (*__bool__) (B_WorldAuth);
    B_str (*__str__) (B_WorldAuth);
    B_str (*__repr__) (B_WorldAuth);
};
struct B_WorldAuth {
    struct B_WorldAuthG_class *$class;
};
B_WorldAuth B_WorldAuthG_new();
extern struct $l$1contG_class $l$1contG_methods;
$l$1cont $l$1contG_new(B_Env, B_str);
extern struct $l$2contG_class $l$2contG_methods;
$l$2cont $l$2contG_new(B_Env, $action);
extern struct $l$3contG_class $l$3contG_methods;
$l$3cont $l$3contG_new(B_Env, B_int);
extern struct B_WorldAuthG_class B_WorldAuthG_methods;
// END GENERATED __builtin__.act
///////////////////////////////////////////////////////////////////////////////////////////

struct B_EnvG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) (B_Env, B_WorldAuth, B_list);
    void (*__serialize__) (B_Env, $Serial$state);
    B_Env (*__deserialize__) (B_Env, $Serial$state);
    B_bool (*__bool__) (B_Env);
    B_str (*__str__) (B_Env);
    B_str (*__repr__) (B_Env);
    $NoneType (*__resume__) (B_Env);
    $R (*stdout_write$local) (B_Env, $Cont, B_str);
    $R (*stdin_install$local) (B_Env, $Cont, $action);
    $R (*exit$local) (B_Env, $Cont, B_int);
    B_Msg (*stdout_write) (B_Env, B_str);
    B_Msg (*stdin_install) (B_Env, $action);
    B_Msg (*exit) (B_Env, B_int);
};
struct B_Env {
    struct B_EnvG_class *$class;
    $Actor $next;
    B_Msg B_Msg;
    B_Msg $outgoing;
    B_Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock B_Msg_lock;
    $long $globkey;
    $int64 $affinity;
    B_WorldAuth auth;
    B_list argv;
};

extern struct B_EnvG_class B_EnvG_methods;
B_Env B_EnvG_newact(B_WorldAuth, B_list);
void D___init__();

void reset_timeout();
