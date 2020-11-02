#include "minienv.h"

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

static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  bzero(fd_data[fd].buffer,BUF_SIZE);
}
int new_socket ($Clos handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}  
void setupConnection (int fd, $str remoteHost) {
  $Connection conn = $NEW($Connection,fd,remoteHost);
  fd_data[fd].chandler->$class->enter(fd_data[fd].chandler,conn); 
}

// l$1lambda ///////////////////////////////////////////////////////////////////////////////////

void l$1lambda$__init__ (l$1lambda l$self, $_EnvActor __self__, $str address, $int port, $Clos on_success) {
  l$self->__self__ = __self__;
  l$self->address = address;
  l$self->port = port;
  l$self->on_success = on_success;
};

void l$1lambda$__serialize__(l$1lambda l$self, $Serial$state state) {
  $step_serialize(l$self->__self__,state);
  $step_serialize(l$self->address,state);
  $step_serialize(l$self->port,state);
  $step_serialize(l$self->on_success,state);
}
 
l$1lambda l$1lambda$__deserialize__($Serial$state state) {
    l$1lambda res = $DNEW(l$1lambda,state);
    res->__self__ = $step_deserialize(state);
    res->address = $step_deserialize(state);
    res->port = $step_deserialize(state);
    res->on_success = $step_deserialize(state);
    return res;
}

$bool l$1lambda$__bool__(l$1lambda self) {
  return $True;
}

$str l$1lambda$__str__(l$1lambda self) {
    char *s;
  asprintf(&s,"<l$1lambda object at %p>",self);
  return to$str(s);
}

void l$1lambda$enter (l$1lambda l$self, $WORD val) {
  $_EnvActor __self__ = l$self->__self__;
  $str address = l$self->address;
  $int port = l$self->port;
  $Clos on_success = l$self->on_success;
  return __self__->$class->do_connect$local(__self__,address, port, on_success);
}

struct l$1lambda$class l$1lambda$methods = {"",UNASSIGNED,NULL,l$1lambda$__init__,l$1lambda$__serialize__,l$1lambda$__deserialize__,
                                            l$1lambda$__bool__,l$1lambda$__str__,l$1lambda$enter};
  
// l$2lambda ///////////////////////////////////////////////////////////////////////////////////

void l$2lambda$__init__ (l$2lambda l$self, $_EnvActor __self__, $int port, $Clos on_success) {
  l$self->__self__ = __self__;
  l$self->port = port;
  l$self->on_success = on_success;
}

void l$2lambda$__serialize__(l$2lambda l$self, $Serial$state state) {
  $step_serialize(l$self->__self__,state);
  $step_serialize(l$self->port,state);
  $step_serialize(l$self->on_success,state);
}
 
l$2lambda l$2lambda$__deserialize__($Serial$state state) {
  l$2lambda res = $DNEW(l$2lambda,state);
  res->__self__ = $step_deserialize(state);
  res->port = $step_deserialize(state);
  res->on_success = $step_deserialize(state);
  return res;
}

$bool l$2lambda$__bool__(l$2lambda self) {
  return $True;
}

$str l$2lambda$__str__(l$2lambda self) {
    char *s;
  asprintf(&s,"<l$2lambda object at %p>",self);
  return to$str(s);
}

void l$2lambda$enter (l$2lambda l$self, $WORD val) {
  $_EnvActor __self__ = l$self->__self__;
  $int port = l$self->port;
  $Clos on_success = l$self->on_success;
  return __self__->$class->do_listen$local(__self__,port, on_success);
}

struct l$2lambda$class l$2lambda$methods = {"",UNASSIGNED,NULL,l$2lambda$__init__,l$2lambda$__serialize__,l$2lambda$__deserialize__,
                                            l$2lambda$__bool__,l$2lambda$__str__,l$2lambda$enter};
  
// l$3lambda ///////////////////////////////////////////////////////////////////////////////////

void l$3lambda$__init__ (l$3lambda l$self, $_EnvActor __self__, $Clos callback) {
  l$self->__self__ = __self__;
  l$self->callback = callback;
}

void l$3lambda$__serialize__(l$3lambda l$self, $Serial$state state) {
  $step_serialize(l$self->__self__,state);
  $step_serialize(l$self->callback,state);
}
 
l$3lambda l$3lambda$__deserialize__($Serial$state state) {
  l$3lambda res = $DNEW(l$3lambda,state);
  res->__self__ = $step_deserialize(state);
  res->callback = $step_deserialize(state);
  return res;
}

$bool l$3lambda$__bool__(l$3lambda self) {
  return $True;
}

$str l$3lambda$__str__(l$3lambda self) {
    char *s;
  asprintf(&s,"<l$3lambda object at %p>",self);
  return to$str(s);
}

void l$3lambda$enter (l$3lambda l$self, $WORD val) {
  $_EnvActor __self__ = l$self->__self__;
  return __self__->$class->do_stdin_install$local(__self__,l$self->callback);
}

struct l$3lambda$class l$3lambda$methods = {"",UNASSIGNED,NULL,l$3lambda$__init__,l$3lambda$__serialize__,l$3lambda$__deserialize__,
                                            l$3lambda$__bool__,l$3lambda$__str__,l$3lambda$enter};
  
// l$4lambda ///////////////////////////////////////////////////////////////////////////////////

void l$4lambda$__init__ (l$4lambda l$self, $_EnvActor __self__, $str str) {
  l$self->__self__ = __self__;
  l$self->str = str;
}

void l$4lambda$__serialize__(l$4lambda l$self, $Serial$state state) {
  $step_serialize(l$self->__self__,state);
  $step_serialize(l$self->str,state);
}
 
l$4lambda l$4lambda$__deserialize__($Serial$state state) {
  l$4lambda res = $DNEW(l$4lambda,state);
  res->__self__ = $step_deserialize(state);
  res->str = $step_deserialize(state);
  return res;
}

$bool l$4lambda$__bool__(l$4lambda self) {
  return $True;
}

$str l$4lambda$__str__(l$4lambda self) {
    char *s;
  asprintf(&s,"<l$4lambda object at %p>",self);
  return to$str(s);
}

void l$4lambda$enter (l$4lambda l$self, $WORD val) {
  $_EnvActor __self__ = l$self->__self__;
  return __self__->$class->do_stdout_write$local(__self__,l$self->str);
}

struct l$4lambda$class l$4lambda$methods = {"",UNASSIGNED,NULL,l$4lambda$__init__,l$4lambda$__serialize__,l$4lambda$__deserialize__,
                                            l$4lambda$__bool__,l$4lambda$__str__,l$4lambda$enter};
  
 
// $_EnvActor //////////////////////////////////////////////////////////////////////////////////////////

$R $_EnvActor$__init__ ($_EnvActor __self__, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $None);
};

void $_EnvActor$do_stdout_write$local($_EnvActor __self__, $str str) {
  printf("%s",str->str);
}

void $_EnvActor$do_stdin_install$local($_EnvActor __self__, $Clos callback) {
  fd_data[STDIN_FILENO].kind = readhandler;
  fd_data[STDIN_FILENO].rhandler = callback;
}
  
void $_EnvActor$do_connect$local ($_EnvActor __self__, $str address, $int port, $Clos on_success) {
  struct sockaddr_in addr;
  struct in_addr iaddr;
  struct hostent *ent;
  int hostid;
  int fd = new_socket(on_success);
  
  ent = gethostbyname((char *)address->str); //this should be replaced by calling getaddrinfo
  if(ent==NULL) {
    fprintf(stderr,"Name lookup error");  // should connect have one more param prescribing what do with errors before connection is established?
    exit(-1);
    //netError(fd,"Name lookup error");
  }
  else {
    memcpy(&hostid, ent->h_addr_list[0], sizeof hostid);
    iaddr.s_addr = hostid;
    fd_data[fd].sock_addr.sin_addr = iaddr;
    fd_data[fd].sock_addr.sin_port = htons(port->val);
    fd_data[fd].sock_addr.sin_family = AF_INET;
    if (connect(fd,(struct sockaddr *)&fd_data[fd].sock_addr,sizeof(struct sockaddr)) < 0) {// couldn't connect immediately, 
      if (errno!=EINPROGRESS)  {                                                            // so check if attempt continues asynchronously.
        fprintf(stderr,"Connect failed");
        exit(-1);
        //netError(fd,"Connect failed");
      }                                            
    } else 
      setupConnection(fd,address);
  }
};

void $_EnvActor$do_listen$local ($_EnvActor __self__, $int port, $Clos on_success) {
  struct sockaddr_in addr;
  int fd = new_socket(on_success);
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(port->val);
  addr.sin_family = AF_INET;
  if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
    perror("bind failed");
  listen(fd,5);
  //  return $NEW($Closable,fd);
};


$R $_EnvActor$do_stdout_write ($_EnvActor __self__, $str str, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)__self__, ($Cont)$NEW(l$4lambda,__self__, str)));
};

$R $_EnvActor$do_stdin_install ($_EnvActor __self__, $str address, $int port, $Clos callback, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)__self__, ($Cont)$NEW(l$3lambda,__self__, callback)));
};

$R $_EnvActor$do_connect ($_EnvActor __self__, $str address, $int port, $Clos on_success, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)__self__, ($Cont)$NEW(l$1lambda,__self__, address, port, on_success)));
};

$R $_EnvActor$do_listen ($_EnvActor __self__, $int port, $Clos on_success, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)__self__,  ($Cont)$NEW(l$2lambda,__self__, port, on_success)));
};


// $_Env //////////////////////////////////////////////////////////////////////////////////////////

void $_Env$__init__ ($_Env self, $_EnvActor actual) {
  self->actual = actual;
};

#define $SKIPRES(x)  x

$R $_Env$stdout_write ($_Env self, $str str, $Clos c$cont) {
  return self->actual->$class->do_stdout_write(self->actual,str, $SKIPRES(c$cont));
};

$R $_Env$stdin_install ($_Env self, $Clos callback, $Clos c$cont) {
  return self->actual->$class->do_stdin_install(self->actual,callback, $SKIPRES(c$cont));
};

$R $_Env$connect ($_Env self, $str address, $int port, $Clos on_success, $Clos c$cont) {
  return self->actual->$class->do_connect(self->actual,address, port, on_success, $SKIPRES(c$cont));
};

$R $_Env$listen ($_Env self, $int port, $Clos on_success, $Clos c$cont) {
  return self->actual->$class->do_listen(self->actual,port, on_success, $SKIPRES(c$cont));
};
