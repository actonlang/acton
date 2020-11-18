#ifndef minienv
#define minienv

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
#include "builtin.h"
#include "rts.h"


#define BUF_SIZE 1024

#define MAX_FD  10

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;

struct FileDescriptorData {
  HandlerCase kind;
  $function rhandler;
  $function errhandler;
  $function chandler;
  struct sockaddr_in sock_addr;
  struct kevent event_spec;
  char buffer[BUF_SIZE];
};

struct FileDescriptorData fd_data[MAX_FD];
int kq;

void setupConnection (int fd);
$str $getName(int fd);

struct minienv$$l$1lambda;
struct minienv$$l$2lambda;
struct minienv$$l$3lambda;
struct minienv$$l$4lambda;
struct minienv$$l$5lambda;
struct minienv$$l$6lambda;
struct minienv$$l$7lambda;
struct minienv$$Env;
struct minienv$$Connection;
typedef struct minienv$$l$1lambda *minienv$$l$1lambda;
typedef struct minienv$$l$2lambda *minienv$$l$2lambda;
typedef struct minienv$$l$3lambda *minienv$$l$3lambda;
typedef struct minienv$$l$4lambda *minienv$$l$4lambda;
typedef struct minienv$$l$5lambda *minienv$$l$5lambda;
typedef struct minienv$$l$6lambda *minienv$$l$6lambda;
typedef struct minienv$$l$7lambda *minienv$$l$7lambda;
typedef struct minienv$$Env *minienv$$Env;
typedef struct minienv$$Connection *minienv$$Connection;
struct minienv$$l$1lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$1lambda, minienv$$Env, $str);
    void* (*__serialize__) (minienv$$l$1lambda, $Serial$state);
    minienv$$l$1lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$1lambda); 
    $bool (*__bool__) (minienv$$l$1lambda); 
    $R (*__call__) (minienv$$l$1lambda, $Cont);
};
struct minienv$$l$1lambda {
    struct minienv$$l$1lambda$class *$class;
    minienv$$Env __self__;
    $str s;
};
struct minienv$$l$2lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$2lambda, minienv$$Env, $function);
    void* (*__serialize__) (minienv$$l$2lambda, $Serial$state);
    minienv$$l$2lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$2lambda); 
    $bool (*__bool__) (minienv$$l$2lambda); 
    $R (*__call__) (minienv$$l$2lambda, $Cont);
};
struct minienv$$l$2lambda {
    struct minienv$$l$2lambda$class *$class;
    minienv$$Env __self__;
    $function cb;
};
struct minienv$$l$3lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$3lambda, minienv$$Env, $str, $int, $function);
    void* (*__serialize__) (minienv$$l$3lambda, $Serial$state);
    minienv$$l$3lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$3lambda); 
    $bool (*__bool__) (minienv$$l$3lambda); 
    $R (*__call__) (minienv$$l$3lambda, $Cont);
};
struct minienv$$l$3lambda {
    struct minienv$$l$3lambda$class *$class;
    minienv$$Env __self__;
    $str host;
    $int port;
    $function cb;
};
struct minienv$$l$4lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$4lambda, minienv$$Env, $int, $function);
    void* (*__serialize__) (minienv$$l$4lambda, $Serial$state);
    minienv$$l$4lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$4lambda); 
    $bool (*__bool__) (minienv$$l$4lambda); 
    $R (*__call__) (minienv$$l$4lambda, $Cont);
};
struct minienv$$l$4lambda {
    struct minienv$$l$4lambda$class *$class;
    minienv$$Env __self__;
    $int port;
    $function cb;
};
struct minienv$$l$5lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$5lambda, minienv$$Connection, $str);
    void* (*__serialize__) (minienv$$l$5lambda, $Serial$state);
    minienv$$l$5lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$5lambda); 
    $bool (*__bool__) (minienv$$l$5lambda); 
    $R (*__call__) (minienv$$l$5lambda, $Cont);
};
struct minienv$$l$5lambda {
    struct minienv$$l$5lambda$class *$class;
    minienv$$Connection __self__;
    $str s;
};
struct minienv$$l$6lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$6lambda, minienv$$Connection);
    void* (*__serialize__) (minienv$$l$6lambda, $Serial$state);
    minienv$$l$6lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$6lambda); 
    $bool (*__bool__) (minienv$$l$6lambda); 
    $R (*__call__) (minienv$$l$6lambda, $Cont);
};
struct minienv$$l$6lambda {
    struct minienv$$l$6lambda$class *$class;
    minienv$$Connection __self__;
};
struct minienv$$l$7lambda$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    void* (*__init__) (minienv$$l$7lambda, minienv$$Connection, $function, $function);
    void* (*__serialize__) (minienv$$l$7lambda, $Serial$state);
    minienv$$l$7lambda (*__deserialize__) ($Serial$state);
    $str (*__str__) (minienv$$l$7lambda); 
    $bool (*__bool__) (minienv$$l$7lambda); 
    $R (*__call__) (minienv$$l$7lambda, $Cont);
};
struct minienv$$l$7lambda {
    struct minienv$$l$7lambda$class *$class;
    minienv$$Connection __self__;
    $function cb1;
    $function cb2;
};
struct minienv$$Env$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
    $R (*__init__) (minienv$$Env, $Cont);
    void* (*__serialize__) (minienv$$Env, $Serial$state);
    minienv$$Env (*__deserialize__) ($Serial$state);
    $bool (*__bool__) (minienv$$Env);
    $str (*__str__) (minienv$$Env);
    $R (*stdout_write$local) (minienv$$Env, $str, $Cont);
    $R (*stdin_install$local) (minienv$$Env, $function, $Cont);
    $R (*connect$local) (minienv$$Env, $str, $int, $function, $Cont);
    $R (*listen$local) (minienv$$Env, $int, $function, $Cont);
    $Msg (*stdout_write) (minienv$$Env, $str);
    $Msg (*stdin_install) (minienv$$Env, $function);
    $Msg (*connect) (minienv$$Env, $str, $int, $function);
    $Msg (*listen) (minienv$$Env, $int, $function);
};
struct minienv$$Env {
    struct minienv$$Env$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    $Lock $msg_lock;
};
struct minienv$$Connection$class {
    char *$GCINFO;
    $int $class_id;
    $Super$class $superclass;
  $R (*__init__) (minienv$$Connection, int, $Cont);
    void* (*__serialize__) (minienv$$Connection, $Serial$state);
    minienv$$Connection (*__deserialize__) ($Serial$state);
    $bool (*__bool__) (minienv$$Connection);
    $str (*__str__) (minienv$$Connection);
    $R (*write$local) (minienv$$Connection, $str, $Cont);
    $R (*close$local) (minienv$$Connection, $Cont);
    $R (*on_receipt$local) (minienv$$Connection, $function, $function, $Cont);
    $Msg (*write) (minienv$$Connection, $str);
    $Msg (*close) (minienv$$Connection);
    $Msg (*on_receipt) (minienv$$Connection, $function, $function);
};
struct minienv$$Connection {
    struct minienv$$Connection$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    $Lock $msg_lock;
    int descriptor;
};
extern struct minienv$$l$1lambda$class minienv$$l$1lambda$methods;
extern struct minienv$$l$2lambda$class minienv$$l$2lambda$methods;
extern struct minienv$$l$3lambda$class minienv$$l$3lambda$methods;
extern struct minienv$$l$4lambda$class minienv$$l$4lambda$methods;
extern struct minienv$$l$5lambda$class minienv$$l$5lambda$methods;
extern struct minienv$$l$6lambda$class minienv$$l$6lambda$methods;
extern struct minienv$$l$7lambda$class minienv$$l$7lambda$methods;
extern struct minienv$$Env$class minienv$$Env$methods;
extern struct minienv$$Connection$class minienv$$Connection$methods;

void minienv$$__init__();
#endif
