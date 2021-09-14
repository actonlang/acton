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
#include <signal.h>

#include "builtin.h"
#include "../rts/rts.h"

#define BUF_SIZE 1024

#define MAX_FD  100

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;

struct minienv$$l$1lambda;
struct minienv$$l$2lambda;
struct minienv$$l$3lambda;
struct minienv$$l$4lambda;
struct minienv$$l$5lambda;
struct minienv$$l$6lambda;
struct minienv$$l$7lambda;
struct minienv$$l$8lambda;
struct minienv$$l$9lambda;
struct minienv$$l$10lambda;
struct minienv$$l$11lambda;
struct minienv$$l$12lambda;
struct minienv$$l$13lambda;
struct minienv$$l$14lambda;
struct $Env;
struct $Connection;
struct $RFile;
struct $WFile;
typedef struct minienv$$l$1lambda *minienv$$l$1lambda;
typedef struct minienv$$l$2lambda *minienv$$l$2lambda;
typedef struct minienv$$l$3lambda *minienv$$l$3lambda;
typedef struct minienv$$l$4lambda *minienv$$l$4lambda;
typedef struct minienv$$l$5lambda *minienv$$l$5lambda;
typedef struct minienv$$l$6lambda *minienv$$l$6lambda;
typedef struct minienv$$l$7lambda *minienv$$l$7lambda;
typedef struct minienv$$l$8lambda *minienv$$l$8lambda;
typedef struct minienv$$l$9lambda *minienv$$l$9lambda;
typedef struct minienv$$l$10lambda *minienv$$l$10lambda;
typedef struct minienv$$l$11lambda *minienv$$l$11lambda;
typedef struct minienv$$l$12lambda *minienv$$l$12lambda;
typedef struct minienv$$l$13lambda *minienv$$l$13lambda;
typedef struct minienv$$l$14lambda *minienv$$l$14lambda;
typedef struct $Env *$Env;
typedef struct $Connection *$Connection;
typedef struct $RFile *$RFile;
typedef struct $WFile *$WFile;

struct FileDescriptorData {
  HandlerCase kind;
  $function rhandler;
  $function errhandler;
  $function chandler;
  $Connection conn;
  struct sockaddr_in sock_addr;
  struct kevent event_spec;
  char buffer[BUF_SIZE];
  int bufnxt;              // only used for RFiles; index of first unreported char
  int bufused;             //        -"-          ; nr of read chars in buffer. Equal to BUF_SIZE except before first read and (possibly) after last read.
};

extern struct FileDescriptorData fd_data[MAX_FD];
extern int kq;

void setupConnection (int fd);
$str $getName(int fd);
void *$eventloop(void *);

//////////////////////////////////////////////////////////////////////////////////////

struct minienv$$l$1lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$1lambda, $Env, $str);
    void (*__serialize__) (minienv$$l$1lambda, $Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$2lambda, $Env, $function);
    void (*__serialize__) (minienv$$l$2lambda, $Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$3lambda, $Env, $str, $int, $function);
    void (*__serialize__) (minienv$$l$3lambda, $Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$4lambda, $Env, $int, $function);
    void (*__serialize__) (minienv$$l$4lambda, $Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$5lambda, $Env, $int);
    void (*__serialize__) (minienv$$l$5lambda, $Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$6lambda, $Env, $str);
    void (*__serialize__) (minienv$$l$6lambda, $Serial$state);
    minienv$$l$6lambda (*__deserialize__) (minienv$$l$6lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$6lambda);
    $str (*__str__) (minienv$$l$6lambda);
    $R (*__call__) (minienv$$l$6lambda, $Cont);
};
struct minienv$$l$6lambda {
    struct minienv$$l$6lambda$class *$class;
    $Env __self__;
    $str nm;
};
struct minienv$$l$7lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$7lambda, $Env, $str);
    void (*__serialize__) (minienv$$l$7lambda, $Serial$state);
    minienv$$l$7lambda (*__deserialize__) (minienv$$l$7lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$7lambda);
    $str (*__str__) (minienv$$l$7lambda);
    $R (*__call__) (minienv$$l$7lambda, $Cont);
};
struct minienv$$l$7lambda {
    struct minienv$$l$7lambda$class *$class;
    $Env __self__;
    $str nm;
};
struct minienv$$l$8lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$8lambda, $Connection, $str);
    void (*__serialize__) (minienv$$l$8lambda, $Serial$state);
    minienv$$l$8lambda (*__deserialize__) (minienv$$l$8lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$8lambda);
    $str (*__str__) (minienv$$l$8lambda);
    $R (*__call__) (minienv$$l$8lambda, $Cont);
};
struct minienv$$l$8lambda {
    struct minienv$$l$8lambda$class *$class;
    $Connection __self__;
    $str s;
};
struct minienv$$l$9lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$9lambda, $Connection);
    void (*__serialize__) (minienv$$l$9lambda, $Serial$state);
    minienv$$l$9lambda (*__deserialize__) (minienv$$l$9lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$9lambda);
    $str (*__str__) (minienv$$l$9lambda);
    $R (*__call__) (minienv$$l$9lambda, $Cont);
};
struct minienv$$l$9lambda {
    struct minienv$$l$9lambda$class *$class;
    $Connection __self__;
};
struct minienv$$l$10lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$10lambda, $Connection, $function, $function);
    void (*__serialize__) (minienv$$l$10lambda, $Serial$state);
    minienv$$l$10lambda (*__deserialize__) (minienv$$l$10lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$10lambda);
    $str (*__str__) (minienv$$l$10lambda);
    $R (*__call__) (minienv$$l$10lambda, $Cont);
};
struct minienv$$l$10lambda {
    struct minienv$$l$10lambda$class *$class;
    $Connection __self__;
    $function cb1;
    $function cb2;
};
struct minienv$$l$11lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$11lambda, $RFile);
    void (*__serialize__) (minienv$$l$11lambda, $Serial$state);
    minienv$$l$11lambda (*__deserialize__) (minienv$$l$11lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$11lambda);
    $str (*__str__) (minienv$$l$11lambda);
    $R (*__call__) (minienv$$l$11lambda, $Cont);
};
struct minienv$$l$11lambda {
    struct minienv$$l$11lambda$class *$class;
    $RFile __self__;
};
struct minienv$$l$12lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$12lambda, $RFile);
    void (*__serialize__) (minienv$$l$12lambda, $Serial$state);
    minienv$$l$12lambda (*__deserialize__) (minienv$$l$12lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$12lambda);
    $str (*__str__) (minienv$$l$12lambda);
    $R (*__call__) (minienv$$l$12lambda, $Cont);
};
struct minienv$$l$12lambda {
    struct minienv$$l$12lambda$class *$class;
    $RFile __self__;
};
struct minienv$$l$13lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$13lambda, $WFile, $str);
    void (*__serialize__) (minienv$$l$13lambda, $Serial$state);
    minienv$$l$13lambda (*__deserialize__) (minienv$$l$13lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$13lambda);
    $str (*__str__) (minienv$$l$13lambda);
    $R (*__call__) (minienv$$l$13lambda, $Cont);
};
struct minienv$$l$13lambda {
    struct minienv$$l$13lambda$class *$class;
    $WFile __self__;
    $str s;
};
struct minienv$$l$14lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (minienv$$l$14lambda, $WFile);
    void (*__serialize__) (minienv$$l$14lambda, $Serial$state);
    minienv$$l$14lambda (*__deserialize__) (minienv$$l$14lambda, $Serial$state);
    $bool (*__bool__) (minienv$$l$14lambda);
    $str (*__str__) (minienv$$l$14lambda);
    $R (*__call__) (minienv$$l$14lambda, $Cont);
};
struct minienv$$l$14lambda {
    struct minienv$$l$14lambda$class *$class;
    $WFile __self__;
};
struct $Env$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) ($Env, $list);
    void (*__serialize__) ($Env, $Serial$state);
    $Env (*__deserialize__) ($Env, $Serial$state);
    $bool (*__bool__) ($Env);
    $str (*__str__) ($Env);
    $R (*stdout_write$local) ($Env, $str, $Cont);
    $R (*stdin_install$local) ($Env, $function, $Cont);
    $R (*connect$local) ($Env, $str, $int, $function, $Cont);
    $R (*listen$local) ($Env, $int, $function, $Cont);
    $R (*exit$local) ($Env, $int, $Cont);
    $R (*openR$local) ($Env, $str, $Cont);
    $R (*openW$local) ($Env, $str, $Cont);
    $Msg (*stdout_write) ($Env, $str);
    $Msg (*stdin_install) ($Env, $function);
    $Msg (*connect) ($Env, $str, $int, $function);
    $Msg (*listen) ($Env, $int, $function);
    $Msg (*exit) ($Env, $int);
    $Msg (*openR) ($Env, $str);
    $Msg (*openW) ($Env, $str);
};
struct $Env {
    struct $Env$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Actor $offspring;
    $Actor $uterus;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    $list argv;
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
    $Actor $offspring;
    $Actor $uterus;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    int descriptor;
};
struct $RFile$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $R (*__init__) ($RFile, FILE*, $Cont);
    void (*__serialize__) ($RFile, $Serial$state);
    $RFile (*__deserialize__) ($RFile, $Serial$state);
    $bool (*__bool__) ($RFile);
    $str (*__str__) ($RFile);
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
    $Actor $offspring;
    $Actor $uterus;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    FILE *file;
};
struct $WFile$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $R (*__init__) ($WFile, int, $Cont);
    void (*__serialize__) ($WFile, $Serial$state);
    $WFile (*__deserialize__) ($WFile, $Serial$state);
    $bool (*__bool__) ($WFile);
    $str (*__str__) ($WFile);
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
    $Actor $offspring;
    $Actor $uterus;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
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
minienv$$l$6lambda minienv$$l$6lambda$new($Env, $str);
extern struct minienv$$l$7lambda$class minienv$$l$7lambda$methods;
minienv$$l$7lambda minienv$$l$7lambda$new($Env, $str);
extern struct minienv$$l$8lambda$class minienv$$l$8lambda$methods;
minienv$$l$8lambda minienv$$l$8lambda$new($Connection, $str);
extern struct minienv$$l$9lambda$class minienv$$l$9lambda$methods;
minienv$$l$9lambda minienv$$l$9lambda$new($Connection);
extern struct minienv$$l$10lambda$class minienv$$l$10lambda$methods;
minienv$$l$10lambda minienv$$l$10lambda$new($Connection, $function, $function);
extern struct minienv$$l$11lambda$class minienv$$l$11lambda$methods;
minienv$$l$11lambda minienv$$l$11lambda$new($RFile);
extern struct minienv$$l$12lambda$class minienv$$l$12lambda$methods;
minienv$$l$12lambda minienv$$l$12lambda$new($RFile);
extern struct minienv$$l$13lambda$class minienv$$l$13lambda$methods;
minienv$$l$13lambda minienv$$l$13lambda$new($WFile, $str);
extern struct minienv$$l$14lambda$class minienv$$l$14lambda$methods;
minienv$$l$14lambda minienv$$l$14lambda$new($WFile);
extern struct $Env$class $Env$methods;
$R $Env$new($list, $Cont);
extern struct $Connection$class $Connection$methods;
$R $Connection$new(int, $Cont);
extern struct $RFile$class $RFile$methods;
$R $RFile$new(FILE*, $Cont);
extern struct $WFile$class $WFile$methods;
$R $WFile$new(int, $Cont);
void minienv$$__init__ ();
