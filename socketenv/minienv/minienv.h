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
#include "../../builtin/builtin.h"
#include "../../rts/rts.h"

struct $MiniEnv;
typedef struct $MiniEnv *$MiniEnv;

struct $MiniEnv$class;
typedef struct $MiniEnv$class *$MiniEnv$class;

struct $Connection;
typedef struct $Connection *$Connection;

struct $Connection$class;
typedef struct $Connection$class *$Connection$class;

struct l$1lambda;
typedef struct l$1lambda *l$1lambda;

struct l$1lambda$class;
typedef struct l$1lambda$class *l$1lambda$class;

struct l$2lambda;
typedef struct l$2lambda *l$2lambda;

struct l$2lambda$class;
typedef struct l$2lambda$class *l$2lambda$class;

struct l$3lambda;
typedef struct l$3lambda *l$3lambda;

struct l$3lambda$class;
typedef struct l$3lambda$class *l$3lambda$class;

struct l$4lambda;
typedef struct l$4lambda *l$4lambda;

struct l$4lambda$class;
typedef struct l$4lambda$class *l$4lambda$class;

struct $_EnvActor;
typedef struct $_EnvActor *$_EnvActor;

struct $_EnvActor$class;
typedef struct $_EnvActor$class *$_EnvActor$class;

struct $_Env;
typedef struct $_Env *$_Env;

struct $_Env$class;
typedef struct $_Env$class *$_Env$class;

// $MiniEnv /////////////////////////////////////////////////////////////////////////////

struct $MiniEnv$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($MiniEnv);
  void (*__serialize__)($MiniEnv, $Serial$state);
  $MiniEnv (*__deserialize__)($Serial$state);
  $bool (*__bool__)($MiniEnv);
  $str (*__str__)($MiniEnv);
  void (*stdout_write)($MiniEnv,$str);
  void (*stdin_install)($MiniEnv,$Clos);
  void (*connect)($MiniEnv,$str,$int,$Clos); 
  void (*listen)($MiniEnv,$int,$Clos); 
};

void $MiniEnv$__init__($MiniEnv, int);
void __serialize__($MiniEnv, $Serial$state);
$MiniEnv $MiniEnv__deserialize__($Serial$state);
$bool $MiniEnv__bool__($MiniEnv);
$str $MiniEnv__str__($MiniEnv);
void $MiniEnv$stdout_write($MiniEnv,$str);
void $MiniEnv$stdin_install($MiniEnv,$Clos);
void $MiniEnv$connect($MiniEnv,$str,$int,$Clos); 
void $MiniEnv$listen($MiniEnv,$int,$Clos); 

struct $MiniEnv {
  struct $MiniEnv$class *$class;
  int kqueue;
};

extern struct $MiniEnv$class $MiniEnv$methods;

 
// $Connection /////////////////////////////////////////////////////////////////////////////

struct $Connection$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Connection,int,$str);
  void (*__serialize__)($Connection, $Serial$state);
  $Connection (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Connection);
  $str (*__str__)($Connection);
  void (*close)($Connection);
  void (*write)($Connection,$str);
  void (*on_receipt)($Connection,$Clos,$Clos);
};

struct $Connection {
  struct $Connection$class *$class;
  int descriptor;
  $str remoteHost;
};

void $Connection$__init__($Connection,int,$str);
void $Connection$__serialize__($Connection, $Serial$state);
$Connection $Connection__deserialize__($Serial$state);
$bool $Connection__bool__($Connection);
$str $Connection__str__($Connection);
void $Connection$on_receipt($Connection,$Clos,$Clos);

extern struct $Connection$class $Connection$methods;

// l$1lambda /////////////////////////////////////////////////////////////////////////////

// closure for connect

struct l$1lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(l$1lambda, $_EnvActor,$str,$int,$Clos);
  void (*__serialize__)(l$1lambda, $Serial$state);
  l$1lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$1lambda);
  $str (*__str__)(l$1lambda);
  void (*enter)(l$1lambda,$WORD);
};

struct l$1lambda {
  struct l$1lambda$class *$class;
  $_EnvActor __self__;
  $str address;
  $int port;
  $Clos on_success;
};

void l$1lambda$__init__(l$1lambda,$_EnvActor,$str,$int,$Clos);
void l$1lambda$__serialize__(l$1lambda, $Serial$state);
l$1lambda l$1lambda__deserialize__($Serial$state);
$bool l$1lambda__bool__(l$1lambda);
$str l$1lambda__str__(l$1lambda);
void l$1lambda$enter(l$1lambda,$WORD);

extern struct l$1lambda$class l$1lambda$methods;

// l$2lambda /////////////////////////////////////////////////////////////////////////////

// closure for listen

struct l$2lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(l$2lambda, $_EnvActor,$int,$Clos);
  void (*__serialize__)(l$2lambda,$Serial$state);
  l$2lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$2lambda);
  $str (*__str__)(l$2lambda);
  void (*enter)(l$2lambda,$WORD);
};

struct l$2lambda {
  struct l$2lambda$class *$class;
  $_EnvActor __self__;
  $int port;
  $Clos on_success;
};

void l$2lambda$__init__(l$2lambda,$_EnvActor,$int,$Clos);
void l$2lambda$__serialize__(l$2lambda, $Serial$state);
l$2lambda l$2lambda__deserialize__($Serial$state);
$bool l$2lambda__bool__(l$2lambda);
$str l$2lambda__str__(l$2lambda);
void l$2lambda$enter(l$2lambda,$WORD);

extern struct l$2lambda$class l$2lambda$methods;

// l$3lambda /////////////////////////////////////////////////////////////////////////////

// closure for stdin_install

struct l$3lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(l$3lambda,$_EnvActor,$Clos);
  void (*__serialize__)(l$3lambda,$Serial$state);
  l$3lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$3lambda);
  $str (*__str__)(l$3lambda);
  void (*enter)(l$3lambda,$WORD);
};

struct l$3lambda {
  struct l$3lambda$class *$class;
  $_EnvActor __self__;
  $Clos callback;
};

void l$3lambda$__init__(l$3lambda,$_EnvActor,$Clos);
void l$3lambda$__serialize__(l$3lambda, $Serial$state);
l$3lambda l$3lambda__deserialize__($Serial$state);
$bool l$3lambda__bool__(l$3lambda);
$str l$3lambda__str__(l$3lambda);
void l$3lambda$enter(l$3lambda,$WORD);

extern struct l$3lambda$class l$3lambda$methods;

// l$4lambda /////////////////////////////////////////////////////////////////////////////

// closure for stdout_write

struct l$4lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(l$4lambda,$_EnvActor,$str);
  void (*__serialize__)(l$4lambda, $Serial$state);
  l$4lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$4lambda);
  $str (*__str__)(l$4lambda);
  void (*enter)(l$4lambda,$WORD);
};

struct l$4lambda {
  struct l$4lambda$class *$class;
  $_EnvActor __self__;
  $str str;
};

void l$4lambda$__init__(l$4lambda,$_EnvActor,$str);
void l$4lambda$__serialize__(l$4lambda, $Serial$state);
l$4lambda l$4lambda__deserialize__($Serial$state);
$bool l$4lambda__bool__(l$4lambda);
$str l$4lambda__str__(l$4lambda);
void l$4lambda$enter(l$4lambda,$WORD);

extern struct l$4lambda$class l$4lambda$methods;

// $_EnvActor //////////////////////////////////////////////////////////////////////////

struct $_EnvActor$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $R (*__init__)($_EnvActor, $Clos);   // !!
  //  void (*__init__)($Actor);
  void (*__serialize__)($_EnvActor, $Serial$state);
  $_EnvActor (*__deserialize__)($Serial$state);
  $bool (*__bool__)($_EnvActor);
  $str (*__str__)($_EnvActor);
  void (*do_stdout_write$local) ($_EnvActor, $str);
  void (*do_stdin_install$local) ($_EnvActor, $Clos);
  void (*do_connect$local) ($_EnvActor, $str, $int, $Clos);
  void (*do_listen$local) ($_EnvActor, $int, $Clos);
  $R (*do_stdout_write) ($_EnvActor, $str, $Clos);
  $R (*do_stdin_install) ($_EnvActor, $Clos, $Clos);
  $R (*do_connect) ($_EnvActor, $str, $int, $Clos, $Clos);
  $R (*do_listen) ($_EnvActor, $int, $Clos, $Clos);
  $R (*do_exit) ($_EnvActor, $int, $Clos);
};

struct $_EnvActor {
  struct $_EnvActor$class *$class;
};

extern struct $_EnvActor$class $_EnvActor$methods;

// $_Env ///////////////////////////////////////////////////////////////////////////////

struct $_Env$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__) ($_Env, $_EnvActor);
  void (*__serialize__)($_Env, $Serial$state);
  $_Env (*__deserialize__)($Serial$state);
  $bool (*__bool__)($_Env);
  $str (*__str__)($_Env);
  $R (*stdout_write) ($_Env, $str, $Clos);
  $R (*stdin_install) ($_Env, $Clos, $Clos);
  $R (*connect) ($_Env, $str, $int, $Clos, $Clos);
  $R (*listen) ($_Env, $int, $Clos, $Clos);
};

struct $_Env {
  struct $_Env$class *$class;
  $_EnvActor actual;
};

extern struct $_Env$class $_Env$methods;
