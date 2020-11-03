#include "minienv.h"

struct $client$$l$1lambda;
typedef struct $client$$l$1lambda *$client$$l$1lambda;

struct $client;
typedef struct $client *$client;

struct $client$$l$1lambda$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $WORD (*__init__)($client$$l$1lambda, $client, $Connection);
  $WORD (*__serialize__)($client$$l$1lambda, $Serial$state);
  $client$$l$1lambda (*__deserialize__)($Serial$state);
  $bool (*__bool__)($client$$l$1lambda);
  $str (*__str__)($client$$l$1lambda);
  $WORD (*__enter__)($client$$l$1lambda);
};

struct $client$$l$1lambda {
  struct $client$$l$1lambda$class *$class;
  $client __self__;
  $Connection conn;
};

extern struct $client$$l$1lambda$class $client$$l$1lambda$methods;

struct $client$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  $R (*__init__)($client, $Env, $Cont);
  $WORD (*__serialize__)($client, $Serial$state);
  $client (*__deserialize__)($Serial$state);
  $bool (*__bool__)($client);
  $str (*__str__)($client);
  $WORD (*session$local) ($client, $Connection);
  $R (*session) ($client, $Connection, $Clos);
};

struct $client {
  struct $client$class *$class;
  $Actor $next;
  $Msg $msg;
  $Msg $outgoing;
  $Catcher $catcher;
  volatile atomic_flag msg_lock;
  $Env env;
};

extern struct $client$class $client$methods;
