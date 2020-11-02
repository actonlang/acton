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

struct $Env;
typedef struct $Env *$Env;

struct $Env$class;
typedef struct $Env$class *$Env$class;

struct $Connection;
typedef struct $Connection *$Connection;

struct $Rd_Handler;
typedef struct $Rd_Handler *$Rd_Handler;

struct $Rd_Handler {
  void(*$handle)($WORD,$str);
  $WORD obj;
};
  
typedef void(*$conhandler)($Connection);

struct $Env$class {
  void (*__init__)($Env,int);
  void (*stdout_write)($Env,$str);
  void (*stdin_install)($Env,$Rd_Handler);
  void (*connect)($Env,$str,$int,$conhandler); 
  void (*listen)($Env,$int,$conhandler); 
};

struct $Env {
  struct $Env$class *$class;
  int kq;
};

extern struct $Env$class $Env$methods;

struct $Connection$class;
typedef struct $Connection$class *$Connection$class;

struct $Connection$class {
  void (*__init__)($Connection,int,$str);
  void (*close)($Connection);
  void (*write)($Connection,$str);
  void (*on_receipt)($Connection,$Rd_Handler,$Rd_Handler);
};

struct $Connection {
  struct $Connection$class *$class;
  int descriptor;
  $str remoteHost;
};

extern struct $Connection$class $Connection$methods;

void event_loop(int kq);
