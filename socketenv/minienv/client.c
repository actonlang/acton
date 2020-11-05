#include "client.h"

$WORD $client$$l$1lambda$__init__($client$$l$1lambda l$self, $client __self__, $Connection conn) {
  l$self->__self__ = __self__;
  l$self->conn = conn;
  return $None;
}

$WORD $client$$l$1lambda$__serialize__($client$$l$1lambda l$self, $Serial$state state) {
  $step_serialize(l$self->__self__,state);
  $step_serialize(l$self->conn,state);
  return $None;
}
 
$client$$l$1lambda $client$$l$1lambda$__deserialize__($Serial$state state) {
  $client$$l$1lambda res = $DNEW($client$$l$1lambda,state);
  res->__self__ = $step_deserialize(state);
  res->conn = $step_deserialize(state);
  return res;
}

$bool $client$$l$1lambda$__bool__($client$$l$1lambda self) {
  return $True;
}

$str $client$$l$1lambda$__str__($client$$l$1lambda self) {
    char *s;
  asprintf(&s,"<$client$$l$1lambda object at %p>",self);
  return to$str(s);
}

$WORD $client$$l$1lambda$__enter__($client$$l$1lambda l$self) {
  return l$self->__self__->$class->session$local( l$self->__self__, l$self->conn);
}

struct $client$$l$1lambda$class $client$$l$1lambda$methods = {"",UNASSIGNED, NULL, $client$$l$1lambda$__init__, $client$$l$1lambda$__serialize__,
                                                              $client$$l$1lambda$__deserialize__, $client$$l$1lambda$__bool__,
                                                              $client$$l$1lambda$__str__, $client$$l$1lambda$__enter__};

$R $client$__init__($client self, $Env env, $Cont c$cont) {
  self->env = env;
  self->env->$class->connect(self->env,to$str("localhost"),to$int(12345),($Clos)self->$class->session$local);
  return $R_CONT(($Cont)c$cont,$None);
}

$WORD $client$__serialize__($client l$self, $Serial$state state) {
  $step_serialize(l$self->$next,state);
  $step_serialize(l$self->$msg,state);
  $step_serialize(l$self->$outgoing,state);
  $step_serialize(l$self->$catcher,state);
  $step_serialize(l$self->env,state);
  return $None;
}

$client $client$__deserialize__($Serial$state state) {
  $client res = $DNEW($client,state);
  res->$next = $step_deserialize(state);
  res->$msg = $step_deserialize(state);
  res->$outgoing = $step_deserialize(state);
  res->$catcher = $step_deserialize(state);
  res->env = $step_deserialize(state);
  atomic_flag_clear(&res->msg_lock);
  return res;
}

$bool $client$__bool__($client self) {
  return $True;
}

$str $client$__str__($client self) {
  char *s;
  asprintf(&s,"<$client object at %p>",self);
  return to$str(s);
}

$WORD $client$session$local($client self, $Connection conn) {
  conn->$class->on_receipt(conn, ($Clos)self->env->$class->stdout_write,  ($Clos)self->env->$class->stdout_write);
  self->env->$class->stdin_install(self->env,($Clos)conn->$class->write);
  return $None;
}

$R $client$session($client self, $Connection conn, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)self,($Cont)$NEW($client$$l$1lambda,self,conn)));
}

struct $client$class $client$methods = {"", UNASSIGNED, NULL, $client$__init__, $client$__serialize__, $client$__deserialize__,
                                        $client$__bool__, $client$__str__, $client$session$local, $client$session};


$R $ROOT($Env env, $Cont then) {
  $register(&$client$$l$1lambda$methods);
  $register(&$client$methods);
  return $NEWCC($client,then,env);
}
