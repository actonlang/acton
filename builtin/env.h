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
struct $l$1lambda;
struct $l$2lambda;
struct $l$3lambda;
struct $WorldAuth;
struct $Env;
struct $Connection;
struct $RFile;
struct $WFile;
struct $ListenSocket;
typedef struct $l$1lambda *$l$1lambda;
typedef struct $l$2lambda *$l$2lambda;
typedef struct $l$3lambda *$l$3lambda;
typedef struct $WorldAuth *$WorldAuth;
typedef struct $Env *$Env;
typedef struct $Connection *$Connection;
typedef struct $RFile *$RFile;
typedef struct $WFile *$WFile;
typedef struct $ListenSocket *$ListenSocket;
struct $l$1lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$1lambda, $Env, $str);
    void (*__serialize__) ($l$1lambda, $Serial$state);
    $l$1lambda (*__deserialize__) ($l$1lambda, $Serial$state);
    $bool (*__bool__) ($l$1lambda);
    $str (*__str__) ($l$1lambda);
    $str (*__repr__) ($l$1lambda);
    $R (*__call__) ($l$1lambda, $Cont);
};
struct $l$1lambda {
    struct $l$1lambda$class *$class;
    $Env __self__;
    $str s;
};
struct $l$2lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$2lambda, $Env, $function);
    void (*__serialize__) ($l$2lambda, $Serial$state);
    $l$2lambda (*__deserialize__) ($l$2lambda, $Serial$state);
    $bool (*__bool__) ($l$2lambda);
    $str (*__str__) ($l$2lambda);
    $str (*__repr__) ($l$2lambda);
    $R (*__call__) ($l$2lambda, $Cont);
};
struct $l$2lambda {
    struct $l$2lambda$class *$class;
    $Env __self__;
    $function cb;
};
struct $l$3lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($l$3lambda, $Env, $int);
    void (*__serialize__) ($l$3lambda, $Serial$state);
    $l$3lambda (*__deserialize__) ($l$3lambda, $Serial$state);
    $bool (*__bool__) ($l$3lambda);
    $str (*__str__) ($l$3lambda);
    $str (*__repr__) ($l$3lambda);
    $R (*__call__) ($l$3lambda, $Cont);
};
struct $l$3lambda {
    struct $l$3lambda$class *$class;
    $Env __self__;
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
extern struct $l$1lambda$class $l$1lambda$methods;
$l$1lambda $l$1lambda$new($Env, $str);
extern struct $l$2lambda$class $l$2lambda$methods;
$l$2lambda $l$2lambda$new($Env, $function);
extern struct $l$3lambda$class $l$3lambda$methods;
$l$3lambda $l$3lambda$new($Env, $int);
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
    $R (*stdout_write$local) ($Env, $str, $Cont);
    $R (*stdin_install$local) ($Env, $function, $Cont);
    $R (*connect$local) ($Env, $str, $int, $function, $Cont);
    $R (*listen$local) ($Env, $int, $function, $function, $Cont);
    $R (*exit$local) ($Env, $int, $Cont);
    $R (*openR$local) ($Env, $str, $Cont);
    $R (*openW$local) ($Env, $str, $Cont);
    $Msg (*stdout_write) ($Env, $str);
    $Msg (*stdin_install) ($Env, $function);
    $Msg (*connect) ($Env, $str, $int, $function);
    $Msg (*listen) ($Env, $int, $function, $function);
    $Msg (*exit) ($Env, $int);
    $Msg (*openR) ($Env, $str);
    $Msg (*openW) ($Env, $str);
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

struct $ListenSocket$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($ListenSocket, int fd, $function on_error);
    void (*__serialize__) ($ListenSocket, $Serial$state);
    $ListenSocket (*__deserialize__) ($ListenSocket, $Serial$state);
    $bool (*__bool__) ($ListenSocket);
    $str (*__str__) ($ListenSocket);
    $str (*__repr__) ($ListenSocket);
    $NoneType (*__resume__) ($ListenSocket);
    $R (*close$local) ($ListenSocket, $Cont);
    $Msg (*close) ($ListenSocket);
};
struct $ListenSocket {
    struct $ListenSocket$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $int64 $affinity;
    int fd;
    $function cb_err;
};

struct $Connection$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($Connection, int);
    void (*__serialize__) ($Connection, $Serial$state);
    $Connection (*__deserialize__) ($Connection, $Serial$state);
    $bool (*__bool__) ($Connection);
    $str (*__str__) ($Connection);
    $str (*__repr__) ($Connection);
    $NoneType (*__resume__) ($Connection);
    $R (*write$local) ($Connection, $bytes, $Cont);
    $R (*close$local) ($Connection, $Cont);
    $R (*on_receive$local) ($Connection, $function, $function, $Cont);
    $Msg (*write) ($Connection, $bytes);
    $Msg (*close) ($Connection);
    $Msg (*on_receive) ($Connection, $function, $function);
};
struct $Connection {
    struct $Connection$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $int64 $affinity;
    int descriptor;
    $function cb_err;
};

struct $RFile$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($RFile, FILE*);
    void (*__serialize__) ($RFile, $Serial$state);
    $RFile (*__deserialize__) ($RFile, $Serial$state);
    $bool (*__bool__) ($RFile);
    $str (*__str__) ($RFile);
    $str (*__repr__) ($RFile);
    $NoneType (*__resume__) ($RFile);
    $R (*readln$local) ($RFile, $Cont);
    $R (*close$local) ($RFile, $Cont);
    $Msg (*readln) ($RFile);
    $Msg (*close) ($RFile);
};
struct $RFile {
    struct $RFile$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $int64 $affinity;
    FILE *file;
};

struct $WFile$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($WFile, int);
    void (*__serialize__) ($WFile, $Serial$state);
    $WFile (*__deserialize__) ($WFile, $Serial$state);
    $bool (*__bool__) ($WFile);
    $str (*__str__) ($WFile);
    $str (*__repr__) ($WFile);
    $NoneType (*__resume__) ($WFile);
    $R (*write$local) ($WFile, $str, $Cont);
    $R (*close$local) ($WFile, $Cont);
    $Msg (*write) ($WFile, $str);
    $Msg (*close) ($WFile);
};
struct $WFile {
    struct $WFile$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $int64 $affinity;
    int descriptor;
};

extern struct $Env$class $Env$methods;
$Env $Env$newact($WorldAuth, $list);
extern struct $Connection$class $Connection$methods;
$Connection $Connection$newact(int);
extern struct $ListenSocket$class $ListenSocket$methods;
$ListenSocket $ListenSocket$newact(int, $function);
extern struct $RFile$class $RFile$methods;
$RFile $RFile$newact(FILE*);
extern struct $WFile$class $WFile$methods;
$WFile $WFile$newact(int);
void $__init__();

void reset_timeout();
