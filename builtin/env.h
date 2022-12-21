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
struct $l$1cont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$1cont, $Env, $str);
    void (*__serialize__) ($l$1cont, $Serial$state);
    $l$1cont (*__deserialize__) ($l$1cont, $Serial$state);
    $bool (*__bool__) ($l$1cont);
    $str (*__str__) ($l$1cont);
    $str (*__repr__) ($l$1cont);
    $R (*__call__) ($l$1cont, $Cont);
};
struct $l$1cont {
    struct $l$1cont$class *$class;
    $Env self;
    $str s;
};
struct $l$2cont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$2cont, $Env, $action);
    void (*__serialize__) ($l$2cont, $Serial$state);
    $l$2cont (*__deserialize__) ($l$2cont, $Serial$state);
    $bool (*__bool__) ($l$2cont);
    $str (*__str__) ($l$2cont);
    $str (*__repr__) ($l$2cont);
    $R (*__call__) ($l$2cont, $Cont);
};
struct $l$2cont {
    struct $l$2cont$class *$class;
    $Env self;
    $action cb;
};
struct $l$3cont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$3cont, $Env, $int);
    void (*__serialize__) ($l$3cont, $Serial$state);
    $l$3cont (*__deserialize__) ($l$3cont, $Serial$state);
    $bool (*__bool__) ($l$3cont);
    $str (*__str__) ($l$3cont);
    $str (*__repr__) ($l$3cont);
    $R (*__call__) ($l$3cont, $Cont);
};
struct $l$3cont {
    struct $l$3cont$class *$class;
    $Env self;
    $int n;
};
struct $WorldAuth$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($WorldAuth);
    void (*__serialize__) ($WorldAuth, $Serial$state);
    $WorldAuth (*__deserialize__) ($WorldAuth, $Serial$state);
    $bool (*__bool__) ($WorldAuth);
    $str (*__str__) ($WorldAuth);
    $str (*__repr__) ($WorldAuth);
};
struct $WorldAuth {
    struct $WorldAuth$class *$class;
};
$WorldAuth $WorldAuth$new();
extern struct $l$1cont$class $l$1cont$methods;
$l$1cont $l$1cont$new($Env, $str);
extern struct $l$2cont$class $l$2cont$methods;
$l$2cont $l$2cont$new($Env, $action);
extern struct $l$3cont$class $l$3cont$methods;
$l$3cont $l$3cont$new($Env, $int);
extern struct $WorldAuth$class $WorldAuth$methods;
// END GENERATED __builtin__.act
///////////////////////////////////////////////////////////////////////////////////////////

struct $Env$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($Env, $WorldAuth, $list);
    void (*__serialize__) ($Env, $Serial$state);
    $Env (*__deserialize__) ($Env, $Serial$state);
    $bool (*__bool__) ($Env);
    $str (*__str__) ($Env);
    $str (*__repr__) ($Env);
    $NoneType (*__resume__) ($Env);
    $R (*stdout_write$local) ($Env, $Cont, $str);
    $R (*stdin_install$local) ($Env, $Cont, $action);
    $R (*exit$local) ($Env, $Cont, $int);
    $Msg (*stdout_write) ($Env, $str);
    $Msg (*stdin_install) ($Env, $action);
    $Msg (*exit) ($Env, $int);
};
struct $Env {
    struct $Env$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $int64 $affinity;
    $WorldAuth auth;
    $list argv;
};

extern struct $Env$class $Env$methods;
$Env $Env$newact($WorldAuth, $list);
void $__init__();

void reset_timeout();
