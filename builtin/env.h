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
struct $WorldAuth;
struct $Env;
typedef struct $l$1cont *$l$1cont;
typedef struct $l$2cont *$l$2cont;
typedef struct $l$3cont *$l$3cont;
typedef struct $WorldAuth *$WorldAuth;
typedef struct $Env *$Env;
struct $l$1contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$1cont, $Env, B_str);
    void (*__serialize__) ($l$1cont, $NoneType);
    $l$1cont (*__deserialize__) ($l$1cont, $NoneType);
    B_bool (*__bool__) ($l$1cont);
    B_str (*__str__) ($l$1cont);
    B_str (*__repr__) ($l$1cont);
    $R (*__call__) ($l$1cont, $Cont);
};
struct $l$1cont {
    struct $l$1contG_class *$class;
    $Env self;
    B_str s;
};
struct $l$2contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$2cont, $Env, $action);
    void (*__serialize__) ($l$2cont, $NoneType);
    $l$2cont (*__deserialize__) ($l$2cont, $NoneType);
    B_bool (*__bool__) ($l$2cont);
    B_str (*__str__) ($l$2cont);
    B_str (*__repr__) ($l$2cont);
    $R (*__call__) ($l$2cont, $Cont);
};
struct $l$2cont {
    struct $l$2contG_class *$class;
    $Env self;
    $action cb;
};
struct $l$3contG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($l$3cont, $Env, B_int);
    void (*__serialize__) ($l$3cont, $NoneType);
    $l$3cont (*__deserialize__) ($l$3cont, $NoneType);
    B_bool (*__bool__) ($l$3cont);
    B_str (*__str__) ($l$3cont);
    B_str (*__repr__) ($l$3cont);
    $R (*__call__) ($l$3cont, $Cont);
};
struct $l$3cont {
    struct $l$3contG_class *$class;
    $Env self;
    B_int n;
};
struct $WorldAuthG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($WorldAuth);
    void (*__serialize__) ($WorldAuth, $NoneType);
    $WorldAuth (*__deserialize__) ($WorldAuth, $NoneType);
    B_bool (*__bool__) ($WorldAuth);
    B_str (*__str__) ($WorldAuth);
    B_str (*__repr__) ($WorldAuth);
};
struct $WorldAuth {
    struct $WorldAuthG_class *$class;
};
$WorldAuth $WorldAuthG_new();
extern struct $l$1contG_class $l$1contG_methods;
$l$1cont $l$1contG_new($Env, B_str);
extern struct $l$2contG_class $l$2contG_methods;
$l$2cont $l$2contG_new($Env, $action);
extern struct $l$3contG_class $l$3contG_methods;
$l$3cont $l$3contG_new($Env, B_int);
extern struct $WorldAuthG_class $WorldAuthG_methods;
// END GENERATED __builtin__.act
///////////////////////////////////////////////////////////////////////////////////////////

struct $EnvG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) ($Env, $WorldAuth, B_list);
    void (*__serialize__) ($Env, $NoneType);
    $Env (*__deserialize__) ($Env, $NoneType);
    B_bool (*__bool__) ($Env);
    B_str (*__str__) ($Env);
    B_str (*__repr__) ($Env);
    $NoneType (*__resume__) ($Env);
    $R (*stdout_write$local) ($Env, $Cont, B_str);
    $R (*stdin_install$local) ($Env, $Cont, $action);
    $R (*exit$local) ($Env, $Cont, B_int);
    $Msg (*stdout_write) ($Env, B_str);
    $Msg (*stdin_install) ($Env, $action);
    $Msg (*exit) ($Env, B_int);
};
struct $Env {
    struct $EnvG_class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    B_int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    B_int64 $affinity;
    $WorldAuth auth;
    B_list argv;
};

extern struct $EnvG_class $EnvG_methods;
$Env $EnvG_newact($WorldAuth, B_list);
void D___init__();

void reset_timeout();
