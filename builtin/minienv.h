#pragma once
//////////////////////////////////////////////////////////////////////////////////
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/event.h>
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

#define SOCK_BUF_SIZE 1024

#define MAX_FD  10

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;

struct FileDescriptorData {
  HandlerCase kind;
  $function rhandler;
  $function errhandler;
  $function chandler;
  struct sockaddr_in sock_addr;
  struct kevent event_spec;
  char buffer[SOCK_BUF_SIZE];
};

struct FileDescriptorData fd_data[MAX_FD];
int kq;

void setupConnection (int fd);
$str $getName(int fd);
void $eventloop();

//////////////////////////////////////////////////////////////////////////////////////
struct minienv$$l$1lambda;
struct minienv$$l$2lambda;
struct minienv$$l$3lambda;
struct minienv$$l$4lambda;
struct minienv$$l$5lambda;
struct minienv$$l$6lambda;
struct minienv$$l$7lambda;
struct minienv$$l$8lambda;
struct $Env;
struct $Connection;
typedef struct minienv$$l$1lambda *minienv$$l$1lambda;
typedef struct minienv$$l$2lambda *minienv$$l$2lambda;
typedef struct minienv$$l$3lambda *minienv$$l$3lambda;
typedef struct minienv$$l$4lambda *minienv$$l$4lambda;
typedef struct minienv$$l$5lambda *minienv$$l$5lambda;
typedef struct minienv$$l$6lambda *minienv$$l$6lambda;
typedef struct minienv$$l$7lambda *minienv$$l$7lambda;
typedef struct minienv$$l$8lambda *minienv$$l$8lambda;
typedef struct $Env *$Env;
typedef struct $Connection *$Connection;
struct minienv$$l$1lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$1lambda, $Env, $str);
    $NoneType (*__serialize__) (minienv$$l$1lambda, $Serial$state);
    minienv$$l$1lambda (*__deserialize__) (minienv$$l$1lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$1lambda);
    $str (*__str__) (minienv$$l$1lambda);
    $R (*__call__) (minienv$$l$1lambda, $Cont);
};
struct minienv$$l$1lambda {
    struct minienv$$l$1lambda$class *$class;
    $Env __self__;
    $str s;
};
struct minienv$$l$2lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$2lambda, $Env, $function);
    $NoneType (*__serialize__) (minienv$$l$2lambda, $Serial$state);
    minienv$$l$2lambda (*__deserialize__) (minienv$$l$2lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$2lambda);
    $str (*__str__) (minienv$$l$2lambda);
    $R (*__call__) (minienv$$l$2lambda, $Cont);
};
struct minienv$$l$2lambda {
    struct minienv$$l$2lambda$class *$class;
    $Env __self__;
    $function cb;
};
struct minienv$$l$3lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$3lambda, $Env, $str, $int, $function);
    $NoneType (*__serialize__) (minienv$$l$3lambda, $Serial$state);
    minienv$$l$3lambda (*__deserialize__) (minienv$$l$3lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$3lambda);
    $str (*__str__) (minienv$$l$3lambda);
    $R (*__call__) (minienv$$l$3lambda, $Cont);
};
struct minienv$$l$3lambda {
    struct minienv$$l$3lambda$class *$class;
    $Env __self__;
    $str host;
    $int port;
    $function cb;
};
struct minienv$$l$4lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$4lambda, $Env, $int, $function);
    $NoneType (*__serialize__) (minienv$$l$4lambda, $Serial$state);
    minienv$$l$4lambda (*__deserialize__) (minienv$$l$4lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$4lambda);
    $str (*__str__) (minienv$$l$4lambda);
    $R (*__call__) (minienv$$l$4lambda, $Cont);
};
struct minienv$$l$4lambda {
    struct minienv$$l$4lambda$class *$class;
    $Env __self__;
    $int port;
    $function cb;
};
struct minienv$$l$5lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$5lambda, $Env, $int);
    $NoneType (*__serialize__) (minienv$$l$5lambda, $Serial$state);
    minienv$$l$5lambda (*__deserialize__) (minienv$$l$5lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$5lambda);
    $str (*__str__) (minienv$$l$5lambda);
    $R (*__call__) (minienv$$l$5lambda, $Cont);
};
struct minienv$$l$5lambda {
    struct minienv$$l$5lambda$class *$class;
    $Env __self__;
    $int n;
};
struct minienv$$l$6lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$6lambda, $Connection, $str);
    $NoneType (*__serialize__) (minienv$$l$6lambda, $Serial$state);
    minienv$$l$6lambda (*__deserialize__) (minienv$$l$6lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$6lambda);
    $str (*__str__) (minienv$$l$6lambda);
    $R (*__call__) (minienv$$l$6lambda, $Cont);
};
struct minienv$$l$6lambda {
    struct minienv$$l$6lambda$class *$class;
    $Connection __self__;
    $str s;
};
struct minienv$$l$7lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$7lambda, $Connection);
    $NoneType (*__serialize__) (minienv$$l$7lambda, $Serial$state);
    minienv$$l$7lambda (*__deserialize__) (minienv$$l$7lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$7lambda);
    $str (*__str__) (minienv$$l$7lambda);
    $R (*__call__) (minienv$$l$7lambda, $Cont);
};
struct minienv$$l$7lambda {
    struct minienv$$l$7lambda$class *$class;
    $Connection __self__;
};
struct minienv$$l$8lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$8lambda, $Connection, $function, $function);
    $NoneType (*__serialize__) (minienv$$l$8lambda, $Serial$state);
    minienv$$l$8lambda (*__deserialize__) (minienv$$l$8lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$8lambda);
    $str (*__str__) (minienv$$l$8lambda);
    $R (*__call__) (minienv$$l$8lambda, $Cont);
};
struct minienv$$l$8lambda {
    struct minienv$$l$8lambda$class *$class;
    $Connection __self__;
    $function cb1;
    $function cb2;
};
struct $Env$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $R (*__init__) ($Env, $list, $Cont);
    $NoneType (*__serialize__) ($Env, $Serial$state);
    $Env (*__deserialize__) ($Env, $Serial$state);
    $bool (*__bool__) ($Env);
    $str (*__str__) ($Env);
    $R (*stdout_write$local) ($Env, $str, $Cont);
    $R (*stdin_install$local) ($Env, $function, $Cont);
    $R (*connect$local) ($Env, $str, $int, $function, $Cont);
    $R (*listen$local) ($Env, $int, $function, $Cont);
    $R (*exit$local) ($Env, $int, $Cont);
    $Msg (*stdout_write) ($Env, $str);
    $Msg (*stdin_install) ($Env, $function);
    $Msg (*connect) ($Env, $str, $int, $function);
    $Msg (*listen) ($Env, $int, $function);
    $Msg (*exit) ($Env, $int);
};
struct $Env {
    struct $Env$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    $Lock $msg_lock;
    $list args;
};
struct $Connection$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $R (*__init__) ($Connection, int, $Cont);
    $NoneType (*__serialize__) ($Connection, $Serial$state);
    $Connection (*__deserialize__) ($Connection, $Serial$state);
    $bool (*__bool__) ($Connection);
    $str (*__str__) ($Connection);
    $R (*write$local) ($Connection, $str, $Cont);
    $R (*close$local) ($Connection, $Cont);
    $R (*on_receipt$local) ($Connection, $function, $function, $Cont);
    $Msg (*write) ($Connection, $str);
    $Msg (*close) ($Connection);
    $Msg (*on_receipt) ($Connection, $function, $function);
};
struct $Connection {
    struct $Connection$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    $Lock $msg_lock;
    int descriptor;
};
extern struct minienv$$l$1lambda$class minienv$$l$1lambda$methods;
minienv$$l$1lambda minienv$$l$1lambda$new($Env, $str);
extern struct minienv$$l$2lambda$class minienv$$l$2lambda$methods;
minienv$$l$2lambda minienv$$l$2lambda$new($Env, $function);
extern struct minienv$$l$3lambda$class minienv$$l$3lambda$methods;
minienv$$l$3lambda minienv$$l$3lambda$new($Env, $str, $int, $function);
extern struct minienv$$l$4lambda$class minienv$$l$4lambda$methods;
minienv$$l$4lambda minienv$$l$4lambda$new($Env, $int, $function);
extern struct minienv$$l$5lambda$class minienv$$l$5lambda$methods;
minienv$$l$5lambda minienv$$l$5lambda$new($Env, $int);
extern struct minienv$$l$6lambda$class minienv$$l$6lambda$methods;
minienv$$l$6lambda minienv$$l$6lambda$new($Connection, $str);
extern struct minienv$$l$7lambda$class minienv$$l$7lambda$methods;
minienv$$l$7lambda minienv$$l$7lambda$new($Connection);
extern struct minienv$$l$8lambda$class minienv$$l$8lambda$methods;
minienv$$l$8lambda minienv$$l$8lambda$new($Connection, $function, $function);
extern struct $Env$class $Env$methods;
$R $Env$new($list,$Cont);
extern struct $Connection$class $Connection$methods;
$R $Connection$new(int,$Cont);
void minienv$$__init__ ();
