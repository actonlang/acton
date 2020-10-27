struct stdenv$Env (object, struct) {
}
struct stdenv$Closable (struct) {
}
struct stdenv$Connection (Closable, struct) {
}
struct stdenv$RFile (Closable, struct) {
}
struct stdenv$WFile (Closable, struct) {
}
struct stdenv$l$1lambda ($Clos[pure, (), None]) {
    void stdenv$__init__ ($WORD l$self, $WORD __self__, $str address, $int port, $Clos on_success) {
        l$self->__self__ = __self__;
        l$self->address = address;
        l$self->port = port;
        l$self->on_success = on_success;
    }
    void stdenv$__enter__ ($WORD l$self) {
        $WORD __self__ = l$self->__self__;
        $str address = l$self->address;
        $int port = l$self->port;
        $Clos on_success = l$self->on_success;
        return __self__->do_connect$local(address, port, on_success);
    }
}
struct stdenv$l$2lambda ($Clos[pure, (), None]) {
    void stdenv$__init__ ($WORD l$self, $WORD __self__, $int port, $Clos on_success) {
        l$self->__self__ = __self__;
        l$self->port = port;
        l$self->on_success = on_success;
    }
    void stdenv$__enter__ ($WORD l$self) {
        $WORD __self__ = l$self->__self__;
        $int port = l$self->port;
        $Clos on_success = l$self->on_success;
        return __self__->do_listen$local(port, on_success);
    }
}
struct stdenv$l$3lambda ($Clos[pure, (), None]) {
    void stdenv$__init__ ($WORD l$self, $WORD __self__, $int status) {
        l$self->__self__ = __self__;
        l$self->status = status;
    }
    void stdenv$__enter__ ($WORD l$self) {
        $WORD __self__ = l$self->__self__;
        $int status = l$self->status;
        return __self__->do_exit$local(status);
    }
}
struct stdenv$l$4lambda ($Clos[pure, (), None]) {
    void stdenv$__init__ ($WORD l$self, $WORD __self__, $str filename) {
        l$self->__self__ = __self__;
        l$self->filename = filename;
    }
    void stdenv$__enter__ ($WORD l$self) {
        $WORD __self__ = l$self->__self__;
        $str filename = l$self->filename;
        return __self__->do_openR$local(filename);
    }
}
struct stdenv$l$5lambda ($Clos[pure, (), None]) {
    void stdenv$__init__ ($WORD l$self, $WORD __self__, $str filename) {
        l$self->__self__ = __self__;
        l$self->filename = filename;
    }
    void stdenv$__enter__ ($WORD l$self) {
        $WORD __self__ = l$self->__self__;
        $str filename = l$self->filename;
        return __self__->do_openW$local(filename);
    }
}
struct stdenv$_EnvActor ($Actor) {
    $R stdenv$__init__ ($WORD __self__, $Clos c$cont) {
        return $R_CONT(c$cont, None);
    }
    void stdenv$do_connect$local ($WORD __self__, $str address, $int port, $Clos on_success) {
    }
    void stdenv$do_listen$local ($WORD __self__, $int port, $Clos on_success) {
    }
    void stdenv$do_exit$local ($WORD __self__, $int status) {
    }
    void stdenv$do_openR$local ($WORD __self__, $str filename) {
    }
    void stdenv$do_openW$local ($WORD __self__, $str filename) {
    }
    $R stdenv$do_connect ($WORD __self__, $str address, $int port, $Clos on_success, $Clos c$cont) {
        return $R_CONT(c$cont, $ASYNC(__self__, l$1lambda(__self__, address, port, on_success)));
    }
    $R stdenv$do_listen ($WORD __self__, $int port, $Clos on_success, $Clos c$cont) {
        return $R_CONT(c$cont, $ASYNC(__self__, l$2lambda(__self__, port, on_success)));
    }
    $R stdenv$do_exit ($WORD __self__, $int status, $Clos c$cont) {
        return $R_CONT(c$cont, $ASYNC(__self__, l$3lambda(__self__, status)));
    }
    $R stdenv$do_openR ($WORD __self__, $str filename, $Clos c$cont) {
        return $R_CONT(c$cont, $ASYNC(__self__, l$4lambda(__self__, filename)));
    }
    $R stdenv$do_openW ($WORD __self__, $str filename, $Clos c$cont) {
        return $R_CONT(c$cont, $ASYNC(__self__, l$5lambda(__self__, filename)));
    }
}
struct stdenv$_Env (Env, object, struct) {
    void stdenv$__init__ ($WORD self, RFile stdin, WFile stdout, list argv, _EnvActor actual) {
        self->stdin = stdin;
        self->stdout = stdout;
        self->argv = argv;
        self->actual = actual;
    }
    $R stdenv$exit ($WORD self, $int status, $Clos c$cont) {
        return self->actual->do_exit(status, $SKIPRES(c$cont));
    }
    $R stdenv$openR ($WORD self, $str filename, $Clos c$cont) {
        return self->actual->do_openR(filename, $SKIPRES(c$cont));
    }
    $R stdenv$openW ($WORD self, $str filename, $Clos c$cont) {
        return self->actual->do_openW(filename, $SKIPRES(c$cont));
    }
    $R stdenv$connect ($WORD self, $str address, $int port, $Clos on_success, $Clos c$cont) {
        return self->actual->do_connect(address, port, on_success, $SKIPRES(c$cont));
    }
    $R stdenv$listen ($WORD self, $int port, $Clos on_success, $Clos c$cont) {
        return self->actual->do_listen(port, on_success, $SKIPRES(c$cont));
    }
}
