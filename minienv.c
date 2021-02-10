#include "modules/actwo/minienv.h"
$NoneType actwo$minienv$$l$1lambda$__init__ (actwo$minienv$$l$1lambda p$self, actwo$minienv$$Env __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R actwo$minienv$$l$1lambda$__call__ (actwo$minienv$$l$1lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->stdout_write$local(__self__, s, c$cont);
}
void actwo$minienv$$l$1lambda$__serialize__ (actwo$minienv$$l$1lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
actwo$minienv$$l$1lambda actwo$minienv$$l$1lambda$__deserialize__ (actwo$minienv$$l$1lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$1lambda));
            self->$class = &actwo$minienv$$l$1lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$1lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$1lambda actwo$minienv$$l$1lambda$new(actwo$minienv$$Env p$1, $str p$2) {
    actwo$minienv$$l$1lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$1lambda));
    $tmp->$class = &actwo$minienv$$l$1lambda$methods;
    actwo$minienv$$l$1lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$1lambda$class actwo$minienv$$l$1lambda$methods;
$NoneType actwo$minienv$$l$2lambda$__init__ (actwo$minienv$$l$2lambda p$self, actwo$minienv$$Env __self__, $function cb) {
    p$self->__self__ = __self__;
    p$self->cb = cb;
    return $None;
}
$R actwo$minienv$$l$2lambda$__call__ (actwo$minienv$$l$2lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $function cb = p$self->cb;
    return __self__->$class->stdin_install$local(__self__, cb, c$cont);
}
void actwo$minienv$$l$2lambda$__serialize__ (actwo$minienv$$l$2lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb, state);
}
actwo$minienv$$l$2lambda actwo$minienv$$l$2lambda$__deserialize__ (actwo$minienv$$l$2lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$2lambda));
            self->$class = &actwo$minienv$$l$2lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$2lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$2lambda actwo$minienv$$l$2lambda$new(actwo$minienv$$Env p$1, $function p$2) {
    actwo$minienv$$l$2lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$2lambda));
    $tmp->$class = &actwo$minienv$$l$2lambda$methods;
    actwo$minienv$$l$2lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$2lambda$class actwo$minienv$$l$2lambda$methods;
$NoneType actwo$minienv$$l$3lambda$__init__ (actwo$minienv$$l$3lambda p$self, actwo$minienv$$Env __self__, $str host, $int port, $function cb) {
    p$self->__self__ = __self__;
    p$self->host = host;
    p$self->port = port;
    p$self->cb = cb;
    return $None;
}
$R actwo$minienv$$l$3lambda$__call__ (actwo$minienv$$l$3lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $str host = p$self->host;
    $int port = p$self->port;
    $function cb = p$self->cb;
    return __self__->$class->connect$local(__self__, host, port, cb, c$cont);
}
void actwo$minienv$$l$3lambda$__serialize__ (actwo$minienv$$l$3lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->host, state);
    $step_serialize(self->port, state);
    $step_serialize(self->cb, state);
}
actwo$minienv$$l$3lambda actwo$minienv$$l$3lambda$__deserialize__ (actwo$minienv$$l$3lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$3lambda));
            self->$class = &actwo$minienv$$l$3lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$3lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->host = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$3lambda actwo$minienv$$l$3lambda$new(actwo$minienv$$Env p$1, $str p$2, $int p$3, $function p$4) {
    actwo$minienv$$l$3lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$3lambda));
    $tmp->$class = &actwo$minienv$$l$3lambda$methods;
    actwo$minienv$$l$3lambda$methods.__init__($tmp, p$1, p$2, p$3, p$4);
    return $tmp;
}
struct actwo$minienv$$l$3lambda$class actwo$minienv$$l$3lambda$methods;
$NoneType actwo$minienv$$l$4lambda$__init__ (actwo$minienv$$l$4lambda p$self, actwo$minienv$$Env __self__, $int port, $function cb) {
    p$self->__self__ = __self__;
    p$self->port = port;
    p$self->cb = cb;
    return $None;
}
$R actwo$minienv$$l$4lambda$__call__ (actwo$minienv$$l$4lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $int port = p$self->port;
    $function cb = p$self->cb;
    return __self__->$class->listen$local(__self__, port, cb, c$cont);
}
void actwo$minienv$$l$4lambda$__serialize__ (actwo$minienv$$l$4lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->port, state);
    $step_serialize(self->cb, state);
}
actwo$minienv$$l$4lambda actwo$minienv$$l$4lambda$__deserialize__ (actwo$minienv$$l$4lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$4lambda));
            self->$class = &actwo$minienv$$l$4lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$4lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->port = $step_deserialize(state);
    self->cb = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$4lambda actwo$minienv$$l$4lambda$new(actwo$minienv$$Env p$1, $int p$2, $function p$3) {
    actwo$minienv$$l$4lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$4lambda));
    $tmp->$class = &actwo$minienv$$l$4lambda$methods;
    actwo$minienv$$l$4lambda$methods.__init__($tmp, p$1, p$2, p$3);
    return $tmp;
}
struct actwo$minienv$$l$4lambda$class actwo$minienv$$l$4lambda$methods;
$NoneType actwo$minienv$$l$5lambda$__init__ (actwo$minienv$$l$5lambda p$self, actwo$minienv$$Env __self__, $int n) {
    p$self->__self__ = __self__;
    p$self->n = n;
    return $None;
}
$R actwo$minienv$$l$5lambda$__call__ (actwo$minienv$$l$5lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $int n = p$self->n;
    return __self__->$class->exit$local(__self__, n, c$cont);
}
void actwo$minienv$$l$5lambda$__serialize__ (actwo$minienv$$l$5lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->n, state);
}
actwo$minienv$$l$5lambda actwo$minienv$$l$5lambda$__deserialize__ (actwo$minienv$$l$5lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$5lambda));
            self->$class = &actwo$minienv$$l$5lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$5lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->n = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$5lambda actwo$minienv$$l$5lambda$new(actwo$minienv$$Env p$1, $int p$2) {
    actwo$minienv$$l$5lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$5lambda));
    $tmp->$class = &actwo$minienv$$l$5lambda$methods;
    actwo$minienv$$l$5lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$5lambda$class actwo$minienv$$l$5lambda$methods;
$NoneType actwo$minienv$$l$6lambda$__init__ (actwo$minienv$$l$6lambda p$self, actwo$minienv$$Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R actwo$minienv$$l$6lambda$__call__ (actwo$minienv$$l$6lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openR$local(__self__, nm, c$cont);
}
void actwo$minienv$$l$6lambda$__serialize__ (actwo$minienv$$l$6lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
actwo$minienv$$l$6lambda actwo$minienv$$l$6lambda$__deserialize__ (actwo$minienv$$l$6lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$6lambda));
            self->$class = &actwo$minienv$$l$6lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$6lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$6lambda actwo$minienv$$l$6lambda$new(actwo$minienv$$Env p$1, $str p$2) {
    actwo$minienv$$l$6lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$6lambda));
    $tmp->$class = &actwo$minienv$$l$6lambda$methods;
    actwo$minienv$$l$6lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$6lambda$class actwo$minienv$$l$6lambda$methods;
$NoneType actwo$minienv$$l$7lambda$__init__ (actwo$minienv$$l$7lambda p$self, actwo$minienv$$Env __self__, $str nm) {
    p$self->__self__ = __self__;
    p$self->nm = nm;
    return $None;
}
$R actwo$minienv$$l$7lambda$__call__ (actwo$minienv$$l$7lambda p$self, $Cont c$cont) {
    actwo$minienv$$Env __self__ = p$self->__self__;
    $str nm = p$self->nm;
    return __self__->$class->openW$local(__self__, nm, c$cont);
}
void actwo$minienv$$l$7lambda$__serialize__ (actwo$minienv$$l$7lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->nm, state);
}
actwo$minienv$$l$7lambda actwo$minienv$$l$7lambda$__deserialize__ (actwo$minienv$$l$7lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$7lambda));
            self->$class = &actwo$minienv$$l$7lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$7lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->nm = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$7lambda actwo$minienv$$l$7lambda$new(actwo$minienv$$Env p$1, $str p$2) {
    actwo$minienv$$l$7lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$7lambda));
    $tmp->$class = &actwo$minienv$$l$7lambda$methods;
    actwo$minienv$$l$7lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$7lambda$class actwo$minienv$$l$7lambda$methods;
$NoneType actwo$minienv$$l$8lambda$__init__ (actwo$minienv$$l$8lambda p$self, actwo$minienv$$Connection __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R actwo$minienv$$l$8lambda$__call__ (actwo$minienv$$l$8lambda p$self, $Cont c$cont) {
    actwo$minienv$$Connection __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void actwo$minienv$$l$8lambda$__serialize__ (actwo$minienv$$l$8lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
actwo$minienv$$l$8lambda actwo$minienv$$l$8lambda$__deserialize__ (actwo$minienv$$l$8lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$8lambda));
            self->$class = &actwo$minienv$$l$8lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$8lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$8lambda actwo$minienv$$l$8lambda$new(actwo$minienv$$Connection p$1, $str p$2) {
    actwo$minienv$$l$8lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$8lambda));
    $tmp->$class = &actwo$minienv$$l$8lambda$methods;
    actwo$minienv$$l$8lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$8lambda$class actwo$minienv$$l$8lambda$methods;
$NoneType actwo$minienv$$l$9lambda$__init__ (actwo$minienv$$l$9lambda p$self, actwo$minienv$$Connection __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R actwo$minienv$$l$9lambda$__call__ (actwo$minienv$$l$9lambda p$self, $Cont c$cont) {
    actwo$minienv$$Connection __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void actwo$minienv$$l$9lambda$__serialize__ (actwo$minienv$$l$9lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
actwo$minienv$$l$9lambda actwo$minienv$$l$9lambda$__deserialize__ (actwo$minienv$$l$9lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$9lambda));
            self->$class = &actwo$minienv$$l$9lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$9lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$9lambda actwo$minienv$$l$9lambda$new(actwo$minienv$$Connection p$1) {
    actwo$minienv$$l$9lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$9lambda));
    $tmp->$class = &actwo$minienv$$l$9lambda$methods;
    actwo$minienv$$l$9lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct actwo$minienv$$l$9lambda$class actwo$minienv$$l$9lambda$methods;
$NoneType actwo$minienv$$l$10lambda$__init__ (actwo$minienv$$l$10lambda p$self, actwo$minienv$$Connection __self__, $function cb1, $function cb2) {
    p$self->__self__ = __self__;
    p$self->cb1 = cb1;
    p$self->cb2 = cb2;
    return $None;
}
$R actwo$minienv$$l$10lambda$__call__ (actwo$minienv$$l$10lambda p$self, $Cont c$cont) {
    actwo$minienv$$Connection __self__ = p$self->__self__;
    $function cb1 = p$self->cb1;
    $function cb2 = p$self->cb2;
    return __self__->$class->on_receipt$local(__self__, cb1, cb2, c$cont);
}
void actwo$minienv$$l$10lambda$__serialize__ (actwo$minienv$$l$10lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->cb1, state);
    $step_serialize(self->cb2, state);
}
actwo$minienv$$l$10lambda actwo$minienv$$l$10lambda$__deserialize__ (actwo$minienv$$l$10lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$10lambda));
            self->$class = &actwo$minienv$$l$10lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$10lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->cb1 = $step_deserialize(state);
    self->cb2 = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$10lambda actwo$minienv$$l$10lambda$new(actwo$minienv$$Connection p$1, $function p$2, $function p$3) {
    actwo$minienv$$l$10lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$10lambda));
    $tmp->$class = &actwo$minienv$$l$10lambda$methods;
    actwo$minienv$$l$10lambda$methods.__init__($tmp, p$1, p$2, p$3);
    return $tmp;
}
struct actwo$minienv$$l$10lambda$class actwo$minienv$$l$10lambda$methods;
$NoneType actwo$minienv$$l$11lambda$__init__ (actwo$minienv$$l$11lambda p$self, actwo$minienv$$RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R actwo$minienv$$l$11lambda$__call__ (actwo$minienv$$l$11lambda p$self, $Cont c$cont) {
    actwo$minienv$$RFile __self__ = p$self->__self__;
    return __self__->$class->read$local(__self__, c$cont);
}
void actwo$minienv$$l$11lambda$__serialize__ (actwo$minienv$$l$11lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
actwo$minienv$$l$11lambda actwo$minienv$$l$11lambda$__deserialize__ (actwo$minienv$$l$11lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$11lambda));
            self->$class = &actwo$minienv$$l$11lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$11lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$11lambda actwo$minienv$$l$11lambda$new(actwo$minienv$$RFile p$1) {
    actwo$minienv$$l$11lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$11lambda));
    $tmp->$class = &actwo$minienv$$l$11lambda$methods;
    actwo$minienv$$l$11lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct actwo$minienv$$l$11lambda$class actwo$minienv$$l$11lambda$methods;
$NoneType actwo$minienv$$l$12lambda$__init__ (actwo$minienv$$l$12lambda p$self, actwo$minienv$$RFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R actwo$minienv$$l$12lambda$__call__ (actwo$minienv$$l$12lambda p$self, $Cont c$cont) {
    actwo$minienv$$RFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void actwo$minienv$$l$12lambda$__serialize__ (actwo$minienv$$l$12lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
actwo$minienv$$l$12lambda actwo$minienv$$l$12lambda$__deserialize__ (actwo$minienv$$l$12lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$12lambda));
            self->$class = &actwo$minienv$$l$12lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$12lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$12lambda actwo$minienv$$l$12lambda$new(actwo$minienv$$RFile p$1) {
    actwo$minienv$$l$12lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$12lambda));
    $tmp->$class = &actwo$minienv$$l$12lambda$methods;
    actwo$minienv$$l$12lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct actwo$minienv$$l$12lambda$class actwo$minienv$$l$12lambda$methods;
$NoneType actwo$minienv$$l$13lambda$__init__ (actwo$minienv$$l$13lambda p$self, actwo$minienv$$WFile __self__, $str s) {
    p$self->__self__ = __self__;
    p$self->s = s;
    return $None;
}
$R actwo$minienv$$l$13lambda$__call__ (actwo$minienv$$l$13lambda p$self, $Cont c$cont) {
    actwo$minienv$$WFile __self__ = p$self->__self__;
    $str s = p$self->s;
    return __self__->$class->write$local(__self__, s, c$cont);
}
void actwo$minienv$$l$13lambda$__serialize__ (actwo$minienv$$l$13lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
    $step_serialize(self->s, state);
}
actwo$minienv$$l$13lambda actwo$minienv$$l$13lambda$__deserialize__ (actwo$minienv$$l$13lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$13lambda));
            self->$class = &actwo$minienv$$l$13lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$13lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    self->s = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$13lambda actwo$minienv$$l$13lambda$new(actwo$minienv$$WFile p$1, $str p$2) {
    actwo$minienv$$l$13lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$13lambda));
    $tmp->$class = &actwo$minienv$$l$13lambda$methods;
    actwo$minienv$$l$13lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct actwo$minienv$$l$13lambda$class actwo$minienv$$l$13lambda$methods;
$NoneType actwo$minienv$$l$14lambda$__init__ (actwo$minienv$$l$14lambda p$self, actwo$minienv$$WFile __self__) {
    p$self->__self__ = __self__;
    return $None;
}
$R actwo$minienv$$l$14lambda$__call__ (actwo$minienv$$l$14lambda p$self, $Cont c$cont) {
    actwo$minienv$$WFile __self__ = p$self->__self__;
    return __self__->$class->close$local(__self__, c$cont);
}
void actwo$minienv$$l$14lambda$__serialize__ (actwo$minienv$$l$14lambda self, $Serial$state state) {
    $step_serialize(self->__self__, state);
}
actwo$minienv$$l$14lambda actwo$minienv$$l$14lambda$__deserialize__ (actwo$minienv$$l$14lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$l$14lambda));
            self->$class = &actwo$minienv$$l$14lambda$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$l$14lambda, state);
    }
    self->__self__ = $step_deserialize(state);
    return self;
}
actwo$minienv$$l$14lambda actwo$minienv$$l$14lambda$new(actwo$minienv$$WFile p$1) {
    actwo$minienv$$l$14lambda $tmp = malloc(sizeof(struct actwo$minienv$$l$14lambda));
    $tmp->$class = &actwo$minienv$$l$14lambda$methods;
    actwo$minienv$$l$14lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct actwo$minienv$$l$14lambda$class actwo$minienv$$l$14lambda$methods;
$R actwo$minienv$$Env$__init__ (actwo$minienv$$Env __self__, $list args, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    $NEWACT((($Actor)__self__));
    __self__->argv = args;
    $OLDACT();
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$stdout_write$local (actwo$minienv$$Env __self__, $str s, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$stdin_install$local (actwo$minienv$$Env __self__, $function cb, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$connect$local (actwo$minienv$$Env __self__, $str host, $int port, $function cb, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$listen$local (actwo$minienv$$Env __self__, $int port, $function cb, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$exit$local (actwo$minienv$$Env __self__, $int n, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Env$openR$local (actwo$minienv$$Env __self__, $str nm, $Cont c$cont) {
    return actwo$minienv$$RFile$new(c$cont);
}
$R actwo$minienv$$Env$openW$local (actwo$minienv$$Env __self__, $str nm, $Cont c$cont) {
    return actwo$minienv$$WFile$new(c$cont);
}
$Msg actwo$minienv$$Env$stdout_write (actwo$minienv$$Env __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$1lambda$new(__self__, s)));
}
$Msg actwo$minienv$$Env$stdin_install (actwo$minienv$$Env __self__, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$2lambda$new(__self__, cb)));
}
$Msg actwo$minienv$$Env$connect (actwo$minienv$$Env __self__, $str host, $int port, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$3lambda$new(__self__, host, port, cb)));
}
$Msg actwo$minienv$$Env$listen (actwo$minienv$$Env __self__, $int port, $function cb) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$4lambda$new(__self__, port, cb)));
}
$Msg actwo$minienv$$Env$exit (actwo$minienv$$Env __self__, $int n) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$5lambda$new(__self__, n)));
}
$Msg actwo$minienv$$Env$openR (actwo$minienv$$Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$6lambda$new(__self__, nm)));
}
$Msg actwo$minienv$$Env$openW (actwo$minienv$$Env __self__, $str nm) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$7lambda$new(__self__, nm)));
}
void actwo$minienv$$Env$__serialize__ (actwo$minienv$$Env self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
    $step_serialize(self->argv, state);
}
actwo$minienv$$Env actwo$minienv$$Env$__deserialize__ (actwo$minienv$$Env self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$Env));
            self->$class = &actwo$minienv$$Env$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$Env, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    self->argv = $step_deserialize(state);
    return self;
}
$R actwo$minienv$$Env$new($list p$1, $Cont p$2) {
    actwo$minienv$$Env $tmp = malloc(sizeof(struct actwo$minienv$$Env));
    $tmp->$class = &actwo$minienv$$Env$methods;
    return actwo$minienv$$Env$methods.__init__($tmp, p$1, $CONSTCONT($tmp, p$2));
}
struct actwo$minienv$$Env$class actwo$minienv$$Env$methods;
$R actwo$minienv$$Connection$__init__ (actwo$minienv$$Connection __self__, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    $NEWACT((($Actor)__self__));
    $OLDACT();
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Connection$write$local (actwo$minienv$$Connection __self__, $str s, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Connection$close$local (actwo$minienv$$Connection __self__, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$Connection$on_receipt$local (actwo$minienv$$Connection __self__, $function cb1, $function cb2, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$Msg actwo$minienv$$Connection$write (actwo$minienv$$Connection __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$8lambda$new(__self__, s)));
}
$Msg actwo$minienv$$Connection$close (actwo$minienv$$Connection __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$9lambda$new(__self__)));
}
$Msg actwo$minienv$$Connection$on_receipt (actwo$minienv$$Connection __self__, $function cb1, $function cb2) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$10lambda$new(__self__, cb1, cb2)));
}
void actwo$minienv$$Connection$__serialize__ (actwo$minienv$$Connection self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
actwo$minienv$$Connection actwo$minienv$$Connection$__deserialize__ (actwo$minienv$$Connection self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$Connection));
            self->$class = &actwo$minienv$$Connection$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$Connection, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$R actwo$minienv$$Connection$new($Cont p$1) {
    actwo$minienv$$Connection $tmp = malloc(sizeof(struct actwo$minienv$$Connection));
    $tmp->$class = &actwo$minienv$$Connection$methods;
    return actwo$minienv$$Connection$methods.__init__($tmp, $CONSTCONT($tmp, p$1));
}
struct actwo$minienv$$Connection$class actwo$minienv$$Connection$methods;
$R actwo$minienv$$RFile$__init__ (actwo$minienv$$RFile __self__, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    $NEWACT((($Actor)__self__));
    $OLDACT();
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$RFile$read$local (actwo$minienv$$RFile __self__, $Cont c$cont) {
    return $R_CONT(c$cont, to$str(""));
}
$R actwo$minienv$$RFile$close$local (actwo$minienv$$RFile __self__, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$Msg actwo$minienv$$RFile$read (actwo$minienv$$RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$11lambda$new(__self__)));
}
$Msg actwo$minienv$$RFile$close (actwo$minienv$$RFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$12lambda$new(__self__)));
}
void actwo$minienv$$RFile$__serialize__ (actwo$minienv$$RFile self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
actwo$minienv$$RFile actwo$minienv$$RFile$__deserialize__ (actwo$minienv$$RFile self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$RFile));
            self->$class = &actwo$minienv$$RFile$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$RFile, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$R actwo$minienv$$RFile$new($Cont p$1) {
    actwo$minienv$$RFile $tmp = malloc(sizeof(struct actwo$minienv$$RFile));
    $tmp->$class = &actwo$minienv$$RFile$methods;
    return actwo$minienv$$RFile$methods.__init__($tmp, $CONSTCONT($tmp, p$1));
}
struct actwo$minienv$$RFile$class actwo$minienv$$RFile$methods;
$R actwo$minienv$$WFile$__init__ (actwo$minienv$$WFile __self__, $Cont c$cont) {
    $Actor$methods.__init__((($Actor)__self__));
    $NEWACT((($Actor)__self__));
    $OLDACT();
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$WFile$write$local (actwo$minienv$$WFile __self__, $str s, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R actwo$minienv$$WFile$close$local (actwo$minienv$$WFile __self__, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$Msg actwo$minienv$$WFile$write (actwo$minienv$$WFile __self__, $str s) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$13lambda$new(__self__, s)));
}
$Msg actwo$minienv$$WFile$close (actwo$minienv$$WFile __self__) {
    return $ASYNC((($Actor)__self__), (($Cont)actwo$minienv$$l$14lambda$new(__self__)));
}
void actwo$minienv$$WFile$__serialize__ (actwo$minienv$$WFile self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
actwo$minienv$$WFile actwo$minienv$$WFile$__deserialize__ (actwo$minienv$$WFile self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct actwo$minienv$$WFile));
            self->$class = &actwo$minienv$$WFile$methods;
            return self;
        }
        self = $DNEW(actwo$minienv$$WFile, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$R actwo$minienv$$WFile$new($Cont p$1) {
    actwo$minienv$$WFile $tmp = malloc(sizeof(struct actwo$minienv$$WFile));
    $tmp->$class = &actwo$minienv$$WFile$methods;
    return actwo$minienv$$WFile$methods.__init__($tmp, $CONSTCONT($tmp, p$1));
}
struct actwo$minienv$$WFile$class actwo$minienv$$WFile$methods;
int actwo$minienv$$done$ = 0;
void actwo$minienv$$__init__ () {
    if (actwo$minienv$$done$) return;
    actwo$minienv$$done$ = 1;
    {
        actwo$minienv$$l$1lambda$methods.$GCINFO = "actwo$minienv$$l$1lambda";
        actwo$minienv$$l$1lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$1lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$1lambda))$value$methods.__bool__;
        actwo$minienv$$l$1lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$1lambda))$value$methods.__str__;
        actwo$minienv$$l$1lambda$methods.__init__ = actwo$minienv$$l$1lambda$__init__;
        actwo$minienv$$l$1lambda$methods.__call__ = actwo$minienv$$l$1lambda$__call__;
        actwo$minienv$$l$1lambda$methods.__serialize__ = actwo$minienv$$l$1lambda$__serialize__;
        actwo$minienv$$l$1lambda$methods.__deserialize__ = actwo$minienv$$l$1lambda$__deserialize__;
        $register(&actwo$minienv$$l$1lambda$methods);
    }
    {
        actwo$minienv$$l$2lambda$methods.$GCINFO = "actwo$minienv$$l$2lambda";
        actwo$minienv$$l$2lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$2lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$2lambda))$value$methods.__bool__;
        actwo$minienv$$l$2lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$2lambda))$value$methods.__str__;
        actwo$minienv$$l$2lambda$methods.__init__ = actwo$minienv$$l$2lambda$__init__;
        actwo$minienv$$l$2lambda$methods.__call__ = actwo$minienv$$l$2lambda$__call__;
        actwo$minienv$$l$2lambda$methods.__serialize__ = actwo$minienv$$l$2lambda$__serialize__;
        actwo$minienv$$l$2lambda$methods.__deserialize__ = actwo$minienv$$l$2lambda$__deserialize__;
        $register(&actwo$minienv$$l$2lambda$methods);
    }
    {
        actwo$minienv$$l$3lambda$methods.$GCINFO = "actwo$minienv$$l$3lambda";
        actwo$minienv$$l$3lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$3lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$3lambda))$value$methods.__bool__;
        actwo$minienv$$l$3lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$3lambda))$value$methods.__str__;
        actwo$minienv$$l$3lambda$methods.__init__ = actwo$minienv$$l$3lambda$__init__;
        actwo$minienv$$l$3lambda$methods.__call__ = actwo$minienv$$l$3lambda$__call__;
        actwo$minienv$$l$3lambda$methods.__serialize__ = actwo$minienv$$l$3lambda$__serialize__;
        actwo$minienv$$l$3lambda$methods.__deserialize__ = actwo$minienv$$l$3lambda$__deserialize__;
        $register(&actwo$minienv$$l$3lambda$methods);
    }
    {
        actwo$minienv$$l$4lambda$methods.$GCINFO = "actwo$minienv$$l$4lambda";
        actwo$minienv$$l$4lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$4lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$4lambda))$value$methods.__bool__;
        actwo$minienv$$l$4lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$4lambda))$value$methods.__str__;
        actwo$minienv$$l$4lambda$methods.__init__ = actwo$minienv$$l$4lambda$__init__;
        actwo$minienv$$l$4lambda$methods.__call__ = actwo$minienv$$l$4lambda$__call__;
        actwo$minienv$$l$4lambda$methods.__serialize__ = actwo$minienv$$l$4lambda$__serialize__;
        actwo$minienv$$l$4lambda$methods.__deserialize__ = actwo$minienv$$l$4lambda$__deserialize__;
        $register(&actwo$minienv$$l$4lambda$methods);
    }
    {
        actwo$minienv$$l$5lambda$methods.$GCINFO = "actwo$minienv$$l$5lambda";
        actwo$minienv$$l$5lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$5lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$5lambda))$value$methods.__bool__;
        actwo$minienv$$l$5lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$5lambda))$value$methods.__str__;
        actwo$minienv$$l$5lambda$methods.__init__ = actwo$minienv$$l$5lambda$__init__;
        actwo$minienv$$l$5lambda$methods.__call__ = actwo$minienv$$l$5lambda$__call__;
        actwo$minienv$$l$5lambda$methods.__serialize__ = actwo$minienv$$l$5lambda$__serialize__;
        actwo$minienv$$l$5lambda$methods.__deserialize__ = actwo$minienv$$l$5lambda$__deserialize__;
        $register(&actwo$minienv$$l$5lambda$methods);
    }
    {
        actwo$minienv$$l$6lambda$methods.$GCINFO = "actwo$minienv$$l$6lambda";
        actwo$minienv$$l$6lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$6lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$6lambda))$value$methods.__bool__;
        actwo$minienv$$l$6lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$6lambda))$value$methods.__str__;
        actwo$minienv$$l$6lambda$methods.__init__ = actwo$minienv$$l$6lambda$__init__;
        actwo$minienv$$l$6lambda$methods.__call__ = actwo$minienv$$l$6lambda$__call__;
        actwo$minienv$$l$6lambda$methods.__serialize__ = actwo$minienv$$l$6lambda$__serialize__;
        actwo$minienv$$l$6lambda$methods.__deserialize__ = actwo$minienv$$l$6lambda$__deserialize__;
        $register(&actwo$minienv$$l$6lambda$methods);
    }
    {
        actwo$minienv$$l$7lambda$methods.$GCINFO = "actwo$minienv$$l$7lambda";
        actwo$minienv$$l$7lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$7lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$7lambda))$value$methods.__bool__;
        actwo$minienv$$l$7lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$7lambda))$value$methods.__str__;
        actwo$minienv$$l$7lambda$methods.__init__ = actwo$minienv$$l$7lambda$__init__;
        actwo$minienv$$l$7lambda$methods.__call__ = actwo$minienv$$l$7lambda$__call__;
        actwo$minienv$$l$7lambda$methods.__serialize__ = actwo$minienv$$l$7lambda$__serialize__;
        actwo$minienv$$l$7lambda$methods.__deserialize__ = actwo$minienv$$l$7lambda$__deserialize__;
        $register(&actwo$minienv$$l$7lambda$methods);
    }
    {
        actwo$minienv$$l$8lambda$methods.$GCINFO = "actwo$minienv$$l$8lambda";
        actwo$minienv$$l$8lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$8lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$8lambda))$value$methods.__bool__;
        actwo$minienv$$l$8lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$8lambda))$value$methods.__str__;
        actwo$minienv$$l$8lambda$methods.__init__ = actwo$minienv$$l$8lambda$__init__;
        actwo$minienv$$l$8lambda$methods.__call__ = actwo$minienv$$l$8lambda$__call__;
        actwo$minienv$$l$8lambda$methods.__serialize__ = actwo$minienv$$l$8lambda$__serialize__;
        actwo$minienv$$l$8lambda$methods.__deserialize__ = actwo$minienv$$l$8lambda$__deserialize__;
        $register(&actwo$minienv$$l$8lambda$methods);
    }
    {
        actwo$minienv$$l$9lambda$methods.$GCINFO = "actwo$minienv$$l$9lambda";
        actwo$minienv$$l$9lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$9lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$9lambda))$value$methods.__bool__;
        actwo$minienv$$l$9lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$9lambda))$value$methods.__str__;
        actwo$minienv$$l$9lambda$methods.__init__ = actwo$minienv$$l$9lambda$__init__;
        actwo$minienv$$l$9lambda$methods.__call__ = actwo$minienv$$l$9lambda$__call__;
        actwo$minienv$$l$9lambda$methods.__serialize__ = actwo$minienv$$l$9lambda$__serialize__;
        actwo$minienv$$l$9lambda$methods.__deserialize__ = actwo$minienv$$l$9lambda$__deserialize__;
        $register(&actwo$minienv$$l$9lambda$methods);
    }
    {
        actwo$minienv$$l$10lambda$methods.$GCINFO = "actwo$minienv$$l$10lambda";
        actwo$minienv$$l$10lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$10lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$10lambda))$value$methods.__bool__;
        actwo$minienv$$l$10lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$10lambda))$value$methods.__str__;
        actwo$minienv$$l$10lambda$methods.__init__ = actwo$minienv$$l$10lambda$__init__;
        actwo$minienv$$l$10lambda$methods.__call__ = actwo$minienv$$l$10lambda$__call__;
        actwo$minienv$$l$10lambda$methods.__serialize__ = actwo$minienv$$l$10lambda$__serialize__;
        actwo$minienv$$l$10lambda$methods.__deserialize__ = actwo$minienv$$l$10lambda$__deserialize__;
        $register(&actwo$minienv$$l$10lambda$methods);
    }
    {
        actwo$minienv$$l$11lambda$methods.$GCINFO = "actwo$minienv$$l$11lambda";
        actwo$minienv$$l$11lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$11lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$11lambda))$value$methods.__bool__;
        actwo$minienv$$l$11lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$11lambda))$value$methods.__str__;
        actwo$minienv$$l$11lambda$methods.__init__ = actwo$minienv$$l$11lambda$__init__;
        actwo$minienv$$l$11lambda$methods.__call__ = actwo$minienv$$l$11lambda$__call__;
        actwo$minienv$$l$11lambda$methods.__serialize__ = actwo$minienv$$l$11lambda$__serialize__;
        actwo$minienv$$l$11lambda$methods.__deserialize__ = actwo$minienv$$l$11lambda$__deserialize__;
        $register(&actwo$minienv$$l$11lambda$methods);
    }
    {
        actwo$minienv$$l$12lambda$methods.$GCINFO = "actwo$minienv$$l$12lambda";
        actwo$minienv$$l$12lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$12lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$12lambda))$value$methods.__bool__;
        actwo$minienv$$l$12lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$12lambda))$value$methods.__str__;
        actwo$minienv$$l$12lambda$methods.__init__ = actwo$minienv$$l$12lambda$__init__;
        actwo$minienv$$l$12lambda$methods.__call__ = actwo$minienv$$l$12lambda$__call__;
        actwo$minienv$$l$12lambda$methods.__serialize__ = actwo$minienv$$l$12lambda$__serialize__;
        actwo$minienv$$l$12lambda$methods.__deserialize__ = actwo$minienv$$l$12lambda$__deserialize__;
        $register(&actwo$minienv$$l$12lambda$methods);
    }
    {
        actwo$minienv$$l$13lambda$methods.$GCINFO = "actwo$minienv$$l$13lambda";
        actwo$minienv$$l$13lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$13lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$13lambda))$value$methods.__bool__;
        actwo$minienv$$l$13lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$13lambda))$value$methods.__str__;
        actwo$minienv$$l$13lambda$methods.__init__ = actwo$minienv$$l$13lambda$__init__;
        actwo$minienv$$l$13lambda$methods.__call__ = actwo$minienv$$l$13lambda$__call__;
        actwo$minienv$$l$13lambda$methods.__serialize__ = actwo$minienv$$l$13lambda$__serialize__;
        actwo$minienv$$l$13lambda$methods.__deserialize__ = actwo$minienv$$l$13lambda$__deserialize__;
        $register(&actwo$minienv$$l$13lambda$methods);
    }
    {
        actwo$minienv$$l$14lambda$methods.$GCINFO = "actwo$minienv$$l$14lambda";
        actwo$minienv$$l$14lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        actwo$minienv$$l$14lambda$methods.__bool__ = ($bool (*) (actwo$minienv$$l$14lambda))$value$methods.__bool__;
        actwo$minienv$$l$14lambda$methods.__str__ = ($str (*) (actwo$minienv$$l$14lambda))$value$methods.__str__;
        actwo$minienv$$l$14lambda$methods.__init__ = actwo$minienv$$l$14lambda$__init__;
        actwo$minienv$$l$14lambda$methods.__call__ = actwo$minienv$$l$14lambda$__call__;
        actwo$minienv$$l$14lambda$methods.__serialize__ = actwo$minienv$$l$14lambda$__serialize__;
        actwo$minienv$$l$14lambda$methods.__deserialize__ = actwo$minienv$$l$14lambda$__deserialize__;
        $register(&actwo$minienv$$l$14lambda$methods);
    }
    {
        actwo$minienv$$Env$methods.$GCINFO = "actwo$minienv$$Env";
        actwo$minienv$$Env$methods.$superclass = ($Super$class)&$Actor$methods;
        actwo$minienv$$Env$methods.__bool__ = ($bool (*) (actwo$minienv$$Env))$Actor$methods.__bool__;
        actwo$minienv$$Env$methods.__str__ = ($str (*) (actwo$minienv$$Env))$Actor$methods.__str__;
        actwo$minienv$$Env$methods.__init__ = actwo$minienv$$Env$__init__;
        actwo$minienv$$Env$methods.stdout_write$local = actwo$minienv$$Env$stdout_write$local;
        actwo$minienv$$Env$methods.stdin_install$local = actwo$minienv$$Env$stdin_install$local;
        actwo$minienv$$Env$methods.connect$local = actwo$minienv$$Env$connect$local;
        actwo$minienv$$Env$methods.listen$local = actwo$minienv$$Env$listen$local;
        actwo$minienv$$Env$methods.exit$local = actwo$minienv$$Env$exit$local;
        actwo$minienv$$Env$methods.openR$local = actwo$minienv$$Env$openR$local;
        actwo$minienv$$Env$methods.openW$local = actwo$minienv$$Env$openW$local;
        actwo$minienv$$Env$methods.stdout_write = actwo$minienv$$Env$stdout_write;
        actwo$minienv$$Env$methods.stdin_install = actwo$minienv$$Env$stdin_install;
        actwo$minienv$$Env$methods.connect = actwo$minienv$$Env$connect;
        actwo$minienv$$Env$methods.listen = actwo$minienv$$Env$listen;
        actwo$minienv$$Env$methods.exit = actwo$minienv$$Env$exit;
        actwo$minienv$$Env$methods.openR = actwo$minienv$$Env$openR;
        actwo$minienv$$Env$methods.openW = actwo$minienv$$Env$openW;
        actwo$minienv$$Env$methods.__serialize__ = actwo$minienv$$Env$__serialize__;
        actwo$minienv$$Env$methods.__deserialize__ = actwo$minienv$$Env$__deserialize__;
        $register(&actwo$minienv$$Env$methods);
    }
    {
        actwo$minienv$$Connection$methods.$GCINFO = "actwo$minienv$$Connection";
        actwo$minienv$$Connection$methods.$superclass = ($Super$class)&$Actor$methods;
        actwo$minienv$$Connection$methods.__bool__ = ($bool (*) (actwo$minienv$$Connection))$Actor$methods.__bool__;
        actwo$minienv$$Connection$methods.__str__ = ($str (*) (actwo$minienv$$Connection))$Actor$methods.__str__;
        actwo$minienv$$Connection$methods.__init__ = actwo$minienv$$Connection$__init__;
        actwo$minienv$$Connection$methods.write$local = actwo$minienv$$Connection$write$local;
        actwo$minienv$$Connection$methods.close$local = actwo$minienv$$Connection$close$local;
        actwo$minienv$$Connection$methods.on_receipt$local = actwo$minienv$$Connection$on_receipt$local;
        actwo$minienv$$Connection$methods.write = actwo$minienv$$Connection$write;
        actwo$minienv$$Connection$methods.close = actwo$minienv$$Connection$close;
        actwo$minienv$$Connection$methods.on_receipt = actwo$minienv$$Connection$on_receipt;
        actwo$minienv$$Connection$methods.__serialize__ = actwo$minienv$$Connection$__serialize__;
        actwo$minienv$$Connection$methods.__deserialize__ = actwo$minienv$$Connection$__deserialize__;
        $register(&actwo$minienv$$Connection$methods);
    }
    {
        actwo$minienv$$RFile$methods.$GCINFO = "actwo$minienv$$RFile";
        actwo$minienv$$RFile$methods.$superclass = ($Super$class)&$Actor$methods;
        actwo$minienv$$RFile$methods.__bool__ = ($bool (*) (actwo$minienv$$RFile))$Actor$methods.__bool__;
        actwo$minienv$$RFile$methods.__str__ = ($str (*) (actwo$minienv$$RFile))$Actor$methods.__str__;
        actwo$minienv$$RFile$methods.__init__ = actwo$minienv$$RFile$__init__;
        actwo$minienv$$RFile$methods.read$local = actwo$minienv$$RFile$read$local;
        actwo$minienv$$RFile$methods.close$local = actwo$minienv$$RFile$close$local;
        actwo$minienv$$RFile$methods.read = actwo$minienv$$RFile$read;
        actwo$minienv$$RFile$methods.close = actwo$minienv$$RFile$close;
        actwo$minienv$$RFile$methods.__serialize__ = actwo$minienv$$RFile$__serialize__;
        actwo$minienv$$RFile$methods.__deserialize__ = actwo$minienv$$RFile$__deserialize__;
        $register(&actwo$minienv$$RFile$methods);
    }
    {
        actwo$minienv$$WFile$methods.$GCINFO = "actwo$minienv$$WFile";
        actwo$minienv$$WFile$methods.$superclass = ($Super$class)&$Actor$methods;
        actwo$minienv$$WFile$methods.__bool__ = ($bool (*) (actwo$minienv$$WFile))$Actor$methods.__bool__;
        actwo$minienv$$WFile$methods.__str__ = ($str (*) (actwo$minienv$$WFile))$Actor$methods.__str__;
        actwo$minienv$$WFile$methods.__init__ = actwo$minienv$$WFile$__init__;
        actwo$minienv$$WFile$methods.write$local = actwo$minienv$$WFile$write$local;
        actwo$minienv$$WFile$methods.close$local = actwo$minienv$$WFile$close$local;
        actwo$minienv$$WFile$methods.write = actwo$minienv$$WFile$write;
        actwo$minienv$$WFile$methods.close = actwo$minienv$$WFile$close;
        actwo$minienv$$WFile$methods.__serialize__ = actwo$minienv$$WFile$__serialize__;
        actwo$minienv$$WFile$methods.__deserialize__ = actwo$minienv$$WFile$__deserialize__;
        $register(&actwo$minienv$$WFile$methods);
    }
}