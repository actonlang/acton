#include "client.h"
$NoneType client$$l$1lambda$__init__ (client$$l$1lambda l$self, minienv$$Connection t$y, client$$client __self__) {
    l$self->t$y = t$y;
    l$self->__self__ = __self__;
    return $None;
}
$R client$$l$1lambda$__call__ (client$$l$1lambda l$self, $Cont c$cont) {
    minienv$$Connection t$y = l$self->t$y;
    client$$client __self__ = l$self->__self__;
    return __self__->$class->session$local(__self__, t$y, ($Cont)c$cont);
}
struct client$$l$1lambda$class client$$l$1lambda$methods;
$NoneType client$$l$2lambda$__init__ (client$$l$2lambda l$self, client$$client __self__) {
    l$self->__self__ = __self__;
    return $None;
}
$Msg client$$l$2lambda$__call__ (client$$l$2lambda l$self, minienv$$Connection t$y) {
    client$$client __self__ = l$self->__self__;
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(client$$l$1lambda, t$y, __self__));
}
struct client$$l$2lambda$class client$$l$2lambda$methods;
$NoneType client$$l$4lambda$__init__ (client$$l$4lambda l$self, minienv$$Env l$3self) {
    l$self->l$3self = l$3self;
    return $None;
}
$NoneType client$$l$4lambda$__call__ (client$$l$4lambda l$self, $str l$1x) {
    minienv$$Env l$3self = l$self->l$3self;
    return ($NoneType)l$3self->$class->stdout_write(l$3self, l$1x);
}
struct client$$l$4lambda$class client$$l$4lambda$methods;
$NoneType client$$l$6lambda$__init__ (client$$l$6lambda l$self, minienv$$Env l$5self) {
    l$self->l$5self = l$5self;
    return $None;
}
$NoneType client$$l$6lambda$__call__ (client$$l$6lambda l$self, $str l$1x) {
    minienv$$Env l$5self = l$self->l$5self;
    return ($NoneType)l$5self->$class->stdout_write(l$5self, l$1x);
}
struct client$$l$6lambda$class client$$l$6lambda$methods;
$NoneType client$$l$8lambda$__init__ (client$$l$8lambda l$self, minienv$$Connection l$7self) {
    l$self->l$7self = l$7self;
    return $None;
}
$NoneType client$$l$8lambda$__call__ (client$$l$8lambda l$self, $str l$1x) {
    minienv$$Connection l$7self = l$self->l$7self;
    return ($NoneType)l$7self->$class->write(l$7self, l$1x);
}
struct client$$l$8lambda$class client$$l$8lambda$methods;
$NoneType client$$l$9lambda$__init__ (client$$l$9lambda l$self, client$$client __self__, minienv$$Connection conn) {
    l$self->__self__ = __self__;
    l$self->conn = conn;
    return $None;
}
$R client$$l$9lambda$__call__ (client$$l$9lambda l$self, $Cont c$cont) {
    client$$client __self__ = l$self->__self__;
    minienv$$Connection conn = l$self->conn;
    return __self__->$class->session$local(__self__, conn, ($Cont)c$cont);
}
struct client$$l$9lambda$class client$$l$9lambda$methods;
$R client$$client$__init__ (client$$client __self__, minienv$$Env env, $Cont c$cont) {
    __self__->env = env;
    $Number w$63 = ($Number)$NEW($Integral$int);
    __self__->env->$class->connect(__self__->env, to$str("localhost"), w$63->$class->__fromatom__(w$63, to$int(12345)), ($function)$NEW(client$$l$2lambda, __self__));
    return $R_CONT(c$cont, $None);
}
$R client$$client$session$local (client$$client __self__, minienv$$Connection conn, $Cont c$cont) {
    conn->$class->on_receipt(conn,  ($function)$NEW(client$$l$4lambda, __self__->env),  ($function)$NEW(client$$l$6lambda, __self__->env));
    __self__->env->$class->stdin_install(__self__->env,  ($function)$NEW(client$$l$8lambda, conn));
    return $R_CONT(c$cont, $None);
}
$Msg client$$client$session (client$$client __self__, minienv$$Connection conn) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(client$$l$9lambda, __self__, conn));
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
        client$$l$4lambda$methods.$GCINFO = "client$$l$4lambda";
        client$$l$4lambda$methods.$superclass = ($Super$class)&$function$methods;
        client$$l$4lambda$methods.__init__ = client$$l$4lambda$__init__;
        client$$l$4lambda$methods.__call__ = client$$l$4lambda$__call__;
        $register(&client$$l$4lambda$methods);
    }
    {
        client$$l$6lambda$methods.$GCINFO = "client$$l$6lambda";
        client$$l$6lambda$methods.$superclass = ($Super$class)&$function$methods;
        client$$l$6lambda$methods.__init__ = client$$l$6lambda$__init__;
        client$$l$6lambda$methods.__call__ = client$$l$6lambda$__call__;
        $register(&client$$l$6lambda$methods);
    }
    {
        client$$l$8lambda$methods.$GCINFO = "client$$l$8lambda";
        client$$l$8lambda$methods.$superclass = ($Super$class)&$function$methods;
        client$$l$8lambda$methods.__init__ = client$$l$8lambda$__init__;
        client$$l$8lambda$methods.__call__ = client$$l$8lambda$__call__;
        $register(&client$$l$8lambda$methods);
    }
    {
        client$$l$9lambda$methods.$GCINFO = "client$$l$9lambda";
        client$$l$9lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        client$$l$9lambda$methods.__init__ = client$$l$9lambda$__init__;
        client$$l$9lambda$methods.__call__ = client$$l$9lambda$__call__;
        $register(&client$$l$9lambda$methods);
    }
    {
        client$$client$methods.$GCINFO = "client$$client";
        client$$client$methods.$superclass = ($Super$class)&$Actor$methods;
        client$$client$methods.__bool__ = ($bool (*) (client$$client))$Actor$methods.__bool__;
        client$$client$methods.__str__ = ($str (*) (client$$client))$Actor$methods.__str__;
        client$$client$methods.__init__ = client$$client$__init__;
        client$$client$methods.session$local = client$$client$session$local;
        client$$client$methods.session = client$$client$session;
        $register(&client$$client$methods);
    }
}

// ROOT is added
$R $ROOT(minienv$$Env env, $Cont then) {
  $register(&client$$l$1lambda$methods);
  $register(&client$$l$2lambda$methods);
  $register(&client$$l$4lambda$methods);
  $register(&client$$l$6lambda$methods);
  $register(&client$$l$8lambda$methods);
  $register(&client$$l$9lambda$methods);
  $register(&minienv$$Env$methods);
  $register(&client$$client$methods);
  return $NEWCC(client$$client,then,env);
}
