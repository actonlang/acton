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
#include "rts.h"

#define BUF_SIZE 1024

#define MAX_FD  10

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;

struct FileDescriptorData {
  HandlerCase kind;
  $Clos rhandler;
  $Clos errhandler;
  $Clos chandler;
  struct sockaddr_in sock_addr;
  struct kevent event_spec;
  char buffer[BUF_SIZE];
};

struct FileDescriptorData fd_data[MAX_FD];

void setupConnection (int fd, $str remoteHost);

//////////////////////////////////////////////////////////////////////////////////

struct $Connection;
typedef struct $Connection *$Connection;

struct $Connection$class;
typedef struct $Connection$class *$Connection$class;

struct $Env;
typedef struct $Env *$Env;

struct $Env$class;
typedef struct $Env$class *$Env$class;

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

// $Connection /////////////////////////////////////////////////////////////////////////////

struct $Connection$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)($Connection,$int,$str);
  $WORD (*__serialize__)($Connection, $Serial$state);
  $Connection (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Connection);
  $str (*__str__)($Connection);
  $WORD (*close)($Connection);
  $WORD (*write)($Connection,$str);
  $WORD (*on_receipt)($Connection,$Clos,$Clos);
};

struct $Connection {
  struct $Connection$class *$class;
  $int descriptor;
  $str remoteHost;
};

extern struct $Connection$class $Connection$methods;

// $Env /////////////////////////////////////////////////////////////////////////////

struct $Env$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)($Env);
  $WORD (*__serialize__)($Env, $Serial$state);
  $Env (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Env);
  $str (*__str__)($Env);
  $WORD (*stdout_write)($Env,$str);
  $WORD (*stdin_install)($Env,$Clos);
  $WORD (*connect)($Env,$str,$int,$Clos); 
  $WORD (*listen)($Env,$int,$Clos); 
};
 
struct $Env {
  struct $Env$class *$class;
};

// extern struct $Env$class $Env$methods;

// l$1lambda /////////////////////////////////////////////////////////////////////////////

// closure for stdout_write

struct l$1lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)(l$1lambda,$_EnvActor,$str);
  $WORD (*__serialize__)(l$1lambda, $Serial$state);
  l$1lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$1lambda);
  $str (*__str__)(l$1lambda);
  $WORD (*__enter__)(l$1lambda,$WORD);
};

struct l$1lambda {
  struct l$1lambda$class *$class;
  $_EnvActor __self__;
  $str str;
};

extern struct l$1lambda$class l$1lambda$methods;

// l$2lambda /////////////////////////////////////////////////////////////////////////////

// closure for stdin_install

struct l$2lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)(l$2lambda,$_EnvActor,$Clos);
  $WORD (*__serialize__)(l$2lambda,$Serial$state);
  l$2lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$2lambda);
  $str (*__str__)(l$2lambda);
  $WORD (*__enter__)(l$2lambda,$WORD);
};

struct l$2lambda {
  struct l$2lambda$class *$class;
  $_EnvActor __self__;
  $Clos callback;
};

extern struct l$2lambda$class l$2lambda$methods;

// l$3lambda /////////////////////////////////////////////////////////////////////////////

// closure for connect

struct l$3lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)(l$3lambda, $_EnvActor,$str,$int,$Clos);
  $WORD (*__serialize__)(l$3lambda, $Serial$state);
  l$3lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$3lambda);
  $str (*__str__)(l$3lambda);
  $WORD (*__enter__)(l$3lambda,$WORD);
};

struct l$3lambda {
  struct l$3lambda$class *$class;
  $_EnvActor __self__;
  $str address;
  $int port;
  $Clos on_success;
};

extern struct l$3lambda$class l$3lambda$methods;

// l$4lambda /////////////////////////////////////////////////////////////////////////////

// closure for listen

struct l$4lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)(l$4lambda, $_EnvActor,$int,$Clos);
  $WORD (*__serialize__)(l$4lambda,$Serial$state);
  l$4lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)(l$4lambda);
  $str (*__str__)(l$4lambda);
  $WORD (*__enter__)(l$4lambda,$WORD);
};

struct l$4lambda {
  struct l$4lambda$class *$class;
  $_EnvActor __self__;
  $int port;
  $Clos on_success;
};

extern struct l$4lambda$class l$4lambda$methods;

// $_EnvActor //////////////////////////////////////////////////////////////////////////

struct $_EnvActor$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $R (*__init__)($_EnvActor, $Clos);   // !!
  $WORD (*__serialize__)($_EnvActor, $Serial$state);
  $_EnvActor (*__deserialize__)($Serial$state);
  $bool (*__bool__)($_EnvActor);
  $str (*__str__)($_EnvActor);
  $WORD (*do_stdout_write$local) ($_EnvActor, $str);
  $WORD (*do_stdin_install$local) ($_EnvActor, $Clos);
  $WORD (*do_connect$local) ($_EnvActor, $str, $int, $Clos);
  $WORD (*do_listen$local) ($_EnvActor, $int, $Clos);
  $R (*do_stdout_write) ($_EnvActor, $str, $Clos);
  $R (*do_stdin_install) ($_EnvActor, $Clos, $Clos);
  $R (*do_connect) ($_EnvActor, $str, $int, $Clos, $Clos);
  $R (*do_listen) ($_EnvActor, $int, $Clos, $Clos);
};

struct $_EnvActor {
  struct $_EnvActor$class *$class;
  $Actor next;
  $Msg msg;
  $Msg outgoing;
  $Catcher catcher;
  volatile atomic_flag msg_lock;
};

extern struct $_EnvActor$class $_EnvActor$methods;

// $_Env ///////////////////////////////////////////////////////////////////////////////

struct $_Env$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__) ($_Env, $_EnvActor);
  $WORD (*__serialize__)($_Env, $Serial$state);
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
