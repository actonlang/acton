#include "client.h"
void* client$$l$1lambda$__init__ (client$$l$1lambda l$self, minienv$$Connection t$y, client$$client __self__) {
    l$self->t$y = t$y;
    l$self->__self__ = __self__;
    return NULL;
}
$R client$$l$1lambda$__call__ (client$$l$1lambda l$self, $Cont c$cont) {
    minienv$$Connection t$y = l$self->t$y;
    client$$client __self__ = l$self->__self__;
    return $APP(client$$client, __self__, session$local, t$y, c$cont);
}
struct client$$l$1lambda$class client$$l$1lambda$methods;
void* client$$l$2lambda$__init__ (client$$l$2lambda l$self, client$$client __self__) {
    l$self->__self__ = __self__;
    return NULL;
}
$Msg client$$l$2lambda$__call__ (client$$l$2lambda l$self, minienv$$Connection t$y) {
    client$$client __self__ = l$self->__self__;
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(client$$l$1lambda, t$y, __self__));
}
struct client$$l$2lambda$class client$$l$2lambda$methods;
void* client$$l$5lambda$__init__ (client$$l$5lambda l$self, minienv$$Connection l$4self) {
    l$self->l$4self = l$4self;
    return NULL;
}
$R client$$l$5lambda$__call__ (client$$l$5lambda l$self, $str l$1x, $Cont l$2x) {
    minienv$$Connection l$4self = l$self->l$4self;
    $APP(minienv$$Connection, l$4self, write, l$1x);
    return $R_CONT(l$2x,$None);
}
struct client$$l$5lambda$class client$$l$5lambda$methods;
$R client$$l$3c$1cont (client$$client __self__, minienv$$Connection conn, $Cont c$cont, $Msg c$2res) {
    $APP(minienv$$Env, __self__->env, stdin_install, ($function)$NEW(client$$l$5lambda, conn));
    return $R_CONT(c$cont,$None);
}
void* client$$l$7lambda$__init__ (client$$l$7lambda l$self, minienv$$Env l$6self) {
    l$self->l$6self = l$6self;
    return NULL;
}
$R client$$l$7lambda$__call__ (client$$l$7lambda l$self, $str l$1x, $Cont l$2x) {
    minienv$$Env l$6self = l$self->l$6self;
    $APP(minienv$$Env, l$6self, stdout_write, l$1x);
    return $R_CONT(l$2x,$None);
}
struct client$$l$7lambda$class client$$l$7lambda$methods;
void* client$$l$9lambda$__init__ (client$$l$9lambda l$self, minienv$$Env l$8self) {
    l$self->l$8self = l$8self;
    return NULL;
}
$R client$$l$9lambda$__call__ (client$$l$9lambda l$self, $str l$1x, $Cont l$2x) {
    minienv$$Env l$8self = l$self->l$8self;
    $APP(minienv$$Env, l$8self, stdout_write, l$1x);
    return $R_CONT(l$2x,$None);
}
struct client$$l$9lambda$class client$$l$9lambda$methods;
void* client$$l$10lambda$__init__ (client$$l$10lambda l$self, client$$client __self__, minienv$$Connection conn, $Cont c$cont) {
    l$self->__self__ = __self__;
    l$self->conn = conn;
    l$self->c$cont = c$cont;
    return NULL;
}
$R client$$l$10lambda$__call__ (client$$l$10lambda l$self, $Msg l$1x) {
    client$$client __self__ = l$self->__self__;
    minienv$$Connection conn = l$self->conn;
    $Cont c$cont = l$self->c$cont;
    return client$$l$3c$1cont(__self__, conn, c$cont, l$1x);
}
struct client$$l$10lambda$class client$$l$10lambda$methods;
void* client$$l$11lambda$__init__ (client$$l$11lambda l$self, client$$client __self__, minienv$$Connection conn) {
    l$self->__self__ = __self__;
    l$self->conn = conn;
    return NULL;
}
$R client$$l$11lambda$__call__ (client$$l$11lambda l$self, $Cont c$cont) {
    client$$client __self__ = l$self->__self__;
    minienv$$Connection conn = l$self->conn;
    return $APP(client$$client, __self__, session$local, conn, c$cont);
}
struct client$$l$11lambda$class client$$l$11lambda$methods;
$R client$$client$__init__ (client$$client __self__, minienv$$Env env, $Cont c$cont) {
    __self__->env = env;
    $Number w$63 = ($Number)$NEW($Integral$int);
    $int port = $APP($Number, w$63, __fromatom__, to$int(12345));
    $APP(minienv$$Env, __self__->env, connect, to$str("localhost"), port, ($function)$NEW(client$$l$2lambda, __self__));
    return $R_CONT(c$cont,$None);
}
$R client$$client$session$local (client$$client __self__, minienv$$Connection conn, $Cont c$cont) {
  $APP(minienv$$Connection, conn, on_receipt, ($function)$NEW(client$$l$7lambda, __self__->env), ($function)$NEW(client$$l$9lambda, __self__->env));
  return $R_CONT(($Cont)$NEW(client$$l$10lambda, __self__, conn, c$cont),$None);
}
$Msg client$$client$session (client$$client __self__, minienv$$Connection conn) {
  return $ASYNC(($Actor)__self__, ($Cont)$NEW(client$$l$11lambda, __self__, conn));
}
struct client$$client$class client$$client$methods;
int client$$done$ = 0;
void client$$__init__ () {
    if (client$$done$) return;
    client$$done$ = 1;
    minienv$$__init__();
    {
        client$$l$1lambda$methods.$GCINFO = "client$$l$1lambda";
        client$$l$1lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$1lambda$methods.__init__ = client$$l$1lambda$__init__;
        client$$l$1lambda$methods.__call__ = client$$l$1lambda$__call__;
        $register(&client$$l$1lambda$methods);
    }
    {
        client$$l$2lambda$methods.$GCINFO = "client$$l$2lambda";
        client$$l$2lambda$methods.$superclass = ($Super$class)&$function$methods;
        client$$l$2lambda$methods.__init__ = client$$l$2lambda$__init__;
        client$$l$2lambda$methods.__call__ = client$$l$2lambda$__call__;
        $register(&client$$l$2lambda$methods);
    }
    {
        client$$l$5lambda$methods.$GCINFO = "client$$l$5lambda";
        client$$l$5lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$5lambda$methods.__init__ = client$$l$5lambda$__init__;
        client$$l$5lambda$methods.__call__ = client$$l$5lambda$__call__;
        $register(&client$$l$5lambda$methods);
    }
    {
        client$$l$7lambda$methods.$GCINFO = "client$$l$7lambda";
        client$$l$7lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$7lambda$methods.__init__ = client$$l$7lambda$__init__;
        client$$l$7lambda$methods.__call__ = client$$l$7lambda$__call__;
        $register(&client$$l$7lambda$methods);
    }
    {
        client$$l$9lambda$methods.$GCINFO = "client$$l$9lambda";
        client$$l$9lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$9lambda$methods.__init__ = client$$l$9lambda$__init__;
        client$$l$9lambda$methods.__call__ = client$$l$9lambda$__call__;
        $register(&client$$l$9lambda$methods);
    }
    {
        client$$l$10lambda$methods.$GCINFO = "client$$l$10lambda";
        client$$l$10lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$10lambda$methods.__init__ = client$$l$10lambda$__init__;
        client$$l$10lambda$methods.__call__ = client$$l$10lambda$__call__;
        $register(&client$$l$10lambda$methods);
    }
    {
        client$$l$11lambda$methods.$GCINFO = "client$$l$11lambda";
        client$$l$11lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$11lambda$methods.__init__ = client$$l$11lambda$__init__;
        client$$l$11lambda$methods.__call__ = client$$l$11lambda$__call__;
        $register(&client$$l$11lambda$methods);
    }
    {
        client$$client$methods.$GCINFO = "client$$client";
        client$$client$methods.$superclass = ($Super$class)&$Actor$methods;
        client$$client$methods.__bool__ =  ($bool (*)(client$$client))$Actor$methods.__bool__;
        client$$client$methods.__str__ = ($str (*)(client$$client))$Actor$methods.__str__;
        client$$client$methods.__init__ = client$$client$__init__;
        client$$client$methods.session$local = client$$client$session$local;
        client$$client$methods.session = client$$client$session;
        $register(&client$$client$methods);
    }
}


$R $ROOT(minienv$$Env env, $Cont then) {
  $register(&client$$l$1lambda$methods);
  $register(&client$$l$2lambda$methods);
  $register(&client$$l$5lambda$methods);
  $register(&client$$l$7lambda$methods);
  $register(&client$$l$9lambda$methods);
  $register(&client$$l$10lambda$methods);
  $register(&client$$l$11lambda$methods);
  $register(&minienv$$Env$methods);
  $register(&client$$client$methods);
  return $NEWCC(client$$client,then,env);
}
