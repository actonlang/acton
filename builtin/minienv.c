#include "minienv.h"

static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  bzero(fd_data[fd].buffer,SOCK_BUF_SIZE);
}

int new_socket ($function handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}

void setupConnection (int fd) {
  $Connection conn = $NEW($Connection,fd);
  fd_data[fd].conn = conn;
  fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, conn);
}

$str $getName(int fd) {
  socklen_t socklen = sizeof(struct sockaddr_in);
  char *buf = malloc(100);
  getnameinfo((struct sockaddr *)&fd_data[fd].sock_addr,socklen,buf,100,NULL,0,0);
  return to$str(buf);
}

$NoneType minienv$$l$1lambda$__init__ (minienv$$l$1lambda l$self, $Env __self__, $str s) {
    l$self->__self__ = __self__;
    l$self->s = s;
    return $None;
}
$R minienv$$l$1lambda$__call__ (minienv$$l$1lambda l$self, $Cont c$cont) {
    $Env __self__ = l$self->__self__;
    $str s = l$self->s;
    return __self__->$class->stdout_write$local(__self__, s, ($Cont)c$cont);
}
minienv$$l$1lambda minienv$$l$1lambda$new($Env C$1par, $str C$2par) {
    minienv$$l$1lambda $tmp = malloc(sizeof(struct minienv$$l$1lambda));
    $tmp->$class = &minienv$$l$1lambda$methods;
    minienv$$l$1lambda$methods.__init__($tmp, C$1par, C$2par);
    return $tmp;
}
struct minienv$$l$1lambda$class minienv$$l$1lambda$methods = {"minienv$$l$1lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$1lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$1lambda$__call__};
;
$NoneType minienv$$l$2lambda$__init__ (minienv$$l$2lambda l$self, $Env __self__, $function cb) {
    l$self->__self__ = __self__;
    l$self->cb = cb;
    return $None;
}
$R minienv$$l$2lambda$__call__ (minienv$$l$2lambda l$self, $Cont c$cont) {
    $Env __self__ = l$self->__self__;
    $function cb = l$self->cb;
    return __self__->$class->stdin_install$local(__self__, cb, ($Cont)c$cont);
}
minienv$$l$2lambda minienv$$l$2lambda$new($Env C$1par, $function C$2par) {
    minienv$$l$2lambda $tmp = malloc(sizeof(struct minienv$$l$2lambda));
    $tmp->$class = &minienv$$l$2lambda$methods;
    minienv$$l$2lambda$methods.__init__($tmp, C$1par, C$2par);
    return $tmp;
}
struct minienv$$l$2lambda$class minienv$$l$2lambda$methods = {"minienv$$l$2lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$2lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$2lambda$__call__};
;
$NoneType minienv$$l$3lambda$__init__ (minienv$$l$3lambda l$self, $Env __self__, $str host, $int port, $function cb) {
    l$self->__self__ = __self__;
    l$self->host = host;
    l$self->port = port;
    l$self->cb = cb;
    return $None;
}
$R minienv$$l$3lambda$__call__ (minienv$$l$3lambda l$self, $Cont c$cont) {
    $Env __self__ = l$self->__self__;
    $str host = l$self->host;
    $int port = l$self->port;
    $function cb = l$self->cb;
    return __self__->$class->connect$local(__self__, host, port, cb, ($Cont)c$cont);
}
minienv$$l$3lambda minienv$$l$3lambda$new($Env C$1par, $str C$2par, $int C$3par, $function C$4par) {
    minienv$$l$3lambda $tmp = malloc(sizeof(struct minienv$$l$3lambda));
    $tmp->$class = &minienv$$l$3lambda$methods;
    minienv$$l$3lambda$methods.__init__($tmp, C$1par, C$2par, C$3par, C$4par);
    return $tmp;
}
struct minienv$$l$3lambda$class minienv$$l$3lambda$methods= {"minienv$$l$3lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$3lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$3lambda$__call__};
;
$NoneType minienv$$l$4lambda$__init__ (minienv$$l$4lambda l$self, $Env __self__, $int port, $function cb) {
    l$self->__self__ = __self__;
    l$self->port = port;
    l$self->cb = cb;
    return $None;
}
$R minienv$$l$4lambda$__call__ (minienv$$l$4lambda l$self, $Cont c$cont) {
    $Env __self__ = l$self->__self__;
    $int port = l$self->port;
    $function cb = l$self->cb;
    return __self__->$class->listen$local(__self__, port, cb, ($Cont)c$cont);
}
minienv$$l$4lambda minienv$$l$4lambda$new($Env C$1par, $int C$2par, $function C$3par) {
    minienv$$l$4lambda $tmp = malloc(sizeof(struct minienv$$l$4lambda));
    $tmp->$class = &minienv$$l$4lambda$methods;
    minienv$$l$4lambda$methods.__init__($tmp, C$1par, C$2par, C$3par);
    return $tmp;
}
struct minienv$$l$4lambda$class minienv$$l$4lambda$methods = {"minienv$$l$4lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$4lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$4lambda$__call__};

$NoneType minienv$$l$5lambda$__init__ (minienv$$l$5lambda l$self, $Env __self__, $int n) {
    l$self->__self__ = __self__;
    l$self->n = n;
    return $None;
}
$R minienv$$l$5lambda$__call__ (minienv$$l$5lambda l$self, $Cont c$cont) {
    $Env __self__ = l$self->__self__;
    $int n = l$self->n;
    return __self__->$class->exit$local(__self__, n, ($Cont)c$cont);
}
minienv$$l$5lambda minienv$$l$5lambda$new($Env C$1par, $int C$2par) {
    minienv$$l$5lambda $tmp = malloc(sizeof(struct minienv$$l$5lambda));
    $tmp->$class = &minienv$$l$5lambda$methods;
    minienv$$l$5lambda$methods.__init__($tmp, C$1par, C$2par);
    return $tmp;
}
struct minienv$$l$5lambda$class minienv$$l$5lambda$methods  = {"minienv$$l$5lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$5lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$5lambda$__call__};

$NoneType minienv$$l$6lambda$__init__ (minienv$$l$6lambda l$self, $Connection __self__, $str s) {
    l$self->__self__ = __self__;
    l$self->s = s;
    return $None;
}
$R minienv$$l$6lambda$__call__ (minienv$$l$6lambda l$self, $Cont c$cont) {
    $Connection __self__ = l$self->__self__;
    $str s = l$self->s;
    return __self__->$class->write$local(__self__, s, ($Cont)c$cont);
}
minienv$$l$6lambda minienv$$l$6lambda$new($Connection C$1par, $str C$2par) {
    minienv$$l$6lambda $tmp = malloc(sizeof(struct minienv$$l$6lambda));
    $tmp->$class = &minienv$$l$6lambda$methods;
    minienv$$l$6lambda$methods.__init__($tmp, C$1par, C$2par);
    return $tmp;
}
struct minienv$$l$6lambda$class minienv$$l$6lambda$methods= {"minienv$$l$6lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$6lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$6lambda$__call__};

$NoneType minienv$$l$7lambda$__init__ (minienv$$l$7lambda l$self, $Connection __self__) {
    l$self->__self__ = __self__;
    return $None;
}
$R minienv$$l$7lambda$__call__ (minienv$$l$7lambda l$self, $Cont c$cont) {
    $Connection __self__ = l$self->__self__;
    return __self__->$class->close$local(__self__, ($Cont)c$cont);
}
minienv$$l$7lambda minienv$$l$7lambda$new($Connection C$1par) {
    minienv$$l$7lambda $tmp = malloc(sizeof(struct minienv$$l$7lambda));
    $tmp->$class = &minienv$$l$7lambda$methods;
    minienv$$l$7lambda$methods.__init__($tmp, C$1par);
    return $tmp;
}
struct minienv$$l$7lambda$class minienv$$l$7lambda$methods= {"minienv$$l$7lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$7lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$7lambda$__call__};
$NoneType minienv$$l$8lambda$__init__ (minienv$$l$8lambda l$self, $Connection __self__, $function cb1, $function cb2) {
    l$self->__self__ = __self__;
    l$self->cb1 = cb1;
    l$self->cb2 = cb2;
    return $None;
}
$R minienv$$l$8lambda$__call__ (minienv$$l$8lambda l$self, $Cont c$cont) {
    $Connection __self__ = l$self->__self__;
    $function cb1 = l$self->cb1;
    $function cb2 = l$self->cb2;
    return __self__->$class->on_receipt$local(__self__, cb1, cb2, ($Cont)c$cont);
}
minienv$$l$8lambda minienv$$l$8lambda$new($Connection C$1par, $function C$2par, $function C$3par) {
    minienv$$l$8lambda $tmp = malloc(sizeof(struct minienv$$l$8lambda));
    $tmp->$class = &minienv$$l$8lambda$methods;
    minienv$$l$8lambda$methods.__init__($tmp, C$1par, C$2par, C$3par);
    return $tmp;
}
struct minienv$$l$8lambda$class minienv$$l$8lambda$methods= {"minienv$$l$8lambda", NULL, ($Super$class)&$Cont$methods, minienv$$l$8lambda$__init__, NULL, NULL, NULL, NULL,  minienv$$l$8lambda$__call__};
$NoneType $Env$__init__ ($Env __self__, $list args) {
    $Actor$methods.__init__(($Actor)__self__);
    __self__->args = args;
    return $None;
}
$R $Env$stdout_write$local ($Env __self__, $str s, $Cont c$cont) {
    printf("%s",s->str);
    return $R_CONT(c$cont, $None);
}
$R $Env$stdin_install$local ($Env __self__, $function cb, $Cont c$cont) {
    fd_data[STDIN_FILENO].kind = readhandler;
    fd_data[STDIN_FILENO].rhandler = cb;
    EV_SET(&fd_data[STDIN_FILENO].event_spec,STDIN_FILENO,EVFILT_READ,EV_ADD,0,0,NULL);
    kevent(kq,&fd_data[STDIN_FILENO].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$R $Env$connect$local ($Env __self__, $str host, $int port, $function cb, $Cont c$cont) {
    struct sockaddr_in addr;
    struct in_addr iaddr;
    struct hostent *ent;
    int hostid;
    int fd = new_socket(cb);
    ent = gethostbyname((char *)host->str); //this should be replaced by calling getaddrinfo
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
      if (connect(fd,(struct sockaddr *)&fd_data[fd].sock_addr,sizeof(struct sockaddr)) < 0) { // couldn't connect immediately, 
        if (errno==EINPROGRESS)  {                                                             // so check if attempt continues asynchronously.
          EV_SET(&fd_data[fd].event_spec,fd,EVFILT_WRITE,EV_ADD | EV_ONESHOT,0,0,NULL);
          kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
        } else {
          fprintf(stderr,"Connect failed");
          exit(-1);
          //netError(fd,"Connect failed");
        }
      } else // connect succeeded immediately (can this ever happen for a non-blocking socket)?)
        setupConnection(fd);
    }
    return $R_CONT(c$cont, $None);
}
$R $Env$listen$local ($Env __self__, $int port, $function cb, $Cont c$cont) {
    struct sockaddr_in addr;
    int fd = new_socket(cb);
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port->val);
    addr.sin_family = AF_INET;
    if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
      perror("bind failed");
    listen(fd,5);
    EV_SET(&fd_data[fd].event_spec,fd,EVFILT_READ,EV_ADD | EV_ONESHOT,0,0,NULL);
    kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$R $Env$exit$local ($Env __self__, $int n, $Cont c$cont) {
    exit(n->val);
    return $R_CONT(c$cont, $None);
}
$Msg $Env$stdout_write ($Env __self__, $str s) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$1lambda$new(__self__, s));
}
$Msg $Env$stdin_install ($Env __self__, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$2lambda$new(__self__, cb));
}
$Msg $Env$connect ($Env __self__, $str host, $int port, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$3lambda$new(__self__, host, port, cb));
}
$Msg $Env$listen ($Env __self__, $int port, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$4lambda$new(__self__, port, cb));
}
$Msg $Env$exit ($Env __self__, $int n) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$5lambda$new(__self__, n));
}
$R $Env$new($list args, $Cont C$1par) {
    $Env $tmp = malloc(sizeof(struct $Env));
    $tmp->$class = &$Env$methods;
    $Env$methods.__init__($tmp, args);
    return $R_CONT(C$1par, $tmp);
}
struct $Env$class $Env$methods;
$NoneType $Connection$__init__ ($Connection __self__, int descriptor) {
    $Actor$methods.__init__(($Actor)__self__);
    $Number w$9 = ($Number)$Integral$int$new();
    __self__->descriptor = descriptor;
    return $None;
}
$R $Connection$write$local ($Connection __self__, $str s, $Cont c$cont) {
    memcpy(fd_data[__self__->descriptor].buffer,s->str,s->nbytes+1);
    int chunk_size = s->nbytes > SOCK_BUF_SIZE ? SOCK_BUF_SIZE : s->nbytes; 
    int r = write(__self__->descriptor,fd_data[__self__->descriptor].buffer,chunk_size);
    //  for now, assume str->nbytes < BUF_SIZE
    return $R_CONT(c$cont, $None);
}
$R $Connection$close$local ($Connection __self__, $Cont c$cont) {
    close(__self__->descriptor); 
    $init_FileDescriptorData(__self__->descriptor);
    return $R_CONT(c$cont, $None);
}
$R $Connection$on_receipt$local ($Connection __self__, $function cb1, $function cb2, $Cont c$cont) {
    fd_data[__self__->descriptor].kind = readhandler;
    fd_data[__self__->descriptor].rhandler = cb1;
    fd_data[__self__->descriptor].errhandler = cb2;
    EV_SET(&fd_data[__self__->descriptor].event_spec,__self__->descriptor,EVFILT_READ,EV_ADD,0,0,NULL);
    kevent(kq,&fd_data[__self__->descriptor].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$Msg $Connection$write ($Connection __self__, $str s) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$6lambda$new(__self__, s));
}
$Msg $Connection$close ($Connection __self__) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$7lambda$new(__self__));
}
$Msg $Connection$on_receipt ($Connection __self__, $function cb1, $function cb2) {
    return $ASYNC(($Actor)__self__, ($Cont)minienv$$l$8lambda$new(__self__, cb1, cb2));
}
$R $Connection$new(int fd, $Cont C$1par) {
    $Connection $tmp = malloc(sizeof(struct $Connection));
    $tmp->$class = &$Connection$methods;
    $Connection$methods.__init__($tmp, fd);
    return $R_CONT(C$1par, $tmp);
}
struct $Connection$class $Connection$methods;
int minienv$$done$ = 0;
void minienv$$__init__ () {
    if (minienv$$done$) return;
    minienv$$done$ = 1;
    {
        minienv$$l$1lambda$methods.$GCINFO = "minienv$$l$1lambda";
        minienv$$l$1lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$1lambda$methods.__init__ = minienv$$l$1lambda$__init__;
        minienv$$l$1lambda$methods.__call__ = minienv$$l$1lambda$__call__;
        $register(&minienv$$l$1lambda$methods);
    }
    {
        minienv$$l$2lambda$methods.$GCINFO = "minienv$$l$2lambda";
        minienv$$l$2lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$2lambda$methods.__init__ = minienv$$l$2lambda$__init__;
        minienv$$l$2lambda$methods.__call__ = minienv$$l$2lambda$__call__;
        $register(&minienv$$l$2lambda$methods);
    }
    {
        minienv$$l$3lambda$methods.$GCINFO = "minienv$$l$3lambda";
        minienv$$l$3lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$3lambda$methods.__init__ = minienv$$l$3lambda$__init__;
        minienv$$l$3lambda$methods.__call__ = minienv$$l$3lambda$__call__;
        $register(&minienv$$l$3lambda$methods);
    }
    {
        minienv$$l$4lambda$methods.$GCINFO = "minienv$$l$4lambda";
        minienv$$l$4lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$4lambda$methods.__init__ = minienv$$l$4lambda$__init__;
        minienv$$l$4lambda$methods.__call__ = minienv$$l$4lambda$__call__;
        $register(&minienv$$l$4lambda$methods);
    }
    {
        minienv$$l$5lambda$methods.$GCINFO = "minienv$$l$5lambda";
        minienv$$l$5lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$5lambda$methods.__init__ = minienv$$l$5lambda$__init__;
        minienv$$l$5lambda$methods.__call__ = minienv$$l$5lambda$__call__;
        $register(&minienv$$l$5lambda$methods);
    }
    {
        minienv$$l$6lambda$methods.$GCINFO = "minienv$$l$6lambda";
        minienv$$l$6lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$6lambda$methods.__init__ = minienv$$l$6lambda$__init__;
        minienv$$l$6lambda$methods.__call__ = minienv$$l$6lambda$__call__;
        $register(&minienv$$l$6lambda$methods);
    }
    {
        minienv$$l$7lambda$methods.$GCINFO = "minienv$$l$7lambda";
        minienv$$l$7lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$7lambda$methods.__init__ = minienv$$l$7lambda$__init__;
        minienv$$l$7lambda$methods.__call__ = minienv$$l$7lambda$__call__;
        $register(&minienv$$l$7lambda$methods);
    }
    {
        minienv$$l$8lambda$methods.$GCINFO = "minienv$$l$8lambda";
        minienv$$l$8lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        minienv$$l$8lambda$methods.__init__ = minienv$$l$8lambda$__init__;
        minienv$$l$8lambda$methods.__call__ = minienv$$l$8lambda$__call__;
        $register(&minienv$$l$8lambda$methods);
    }
    { 
        $Env$methods.$GCINFO = "$Env";
        $Env$methods.$superclass = ($Super$class)&$Actor$methods;
        $Env$methods.__serialize__ = ($NoneType (*) ($Env, $Serial$state))$Actor$methods.__serialize__;
        $Env$methods.__deserialize__ = ($Env (*) ($Env, $Serial$state))$Actor$methods.__deserialize__;
        $Env$methods.__bool__ = ($bool (*) ($Env))$Actor$methods.__bool__;
        $Env$methods.__str__ = ($str (*) ($Env))$Actor$methods.__str__;
        $Env$methods.__init__ = $Env$__init__;
        $Env$methods.stdout_write$local = $Env$stdout_write$local;
        $Env$methods.stdin_install$local = $Env$stdin_install$local;
        $Env$methods.connect$local = $Env$connect$local;
        $Env$methods.listen$local = $Env$listen$local;
        $Env$methods.exit$local = $Env$exit$local;
        $Env$methods.stdout_write = $Env$stdout_write;
        $Env$methods.stdin_install = $Env$stdin_install;
        $Env$methods.connect = $Env$connect;
        $Env$methods.listen = $Env$listen;
        $Env$methods.exit = $Env$exit;
        $register(&$Env$methods);
    }
    {
        $Connection$methods.$GCINFO = "$Connection";
        $Connection$methods.$superclass = ($Super$class)&$Actor$methods;
        $Connection$methods.__bool__ = ($bool (*) ($Connection))$Actor$methods.__bool__;
        $Connection$methods.__str__ = ($str (*) ($Connection))$Actor$methods.__str__;
        $Connection$methods.__init__ = $Connection$__init__;
        $Connection$methods.write$local = $Connection$write$local;
        $Connection$methods.close$local = $Connection$close$local;
        $Connection$methods.on_receipt$local = $Connection$on_receipt$local;
        $Connection$methods.write = $Connection$write;
        $Connection$methods.close = $Connection$close;
        $Connection$methods.on_receipt = $Connection$on_receipt;
        $register(&$Connection$methods);
    }
}

void $eventloop() {
  while(1) {
    struct kevent timer;
    pthread_setspecific(self_key, NULL);
    EV_SET(&timer, 9999, EVFILT_TIMER, EV_ADD | EV_ONESHOT, 0, 500, 0);
    kevent(kq,&timer,1,0,0,0);
    struct kevent kev;
    struct sockaddr_in addr;
    socklen_t socklen = sizeof(addr);
    int fd2;
    int count;
    int nready = kevent(kq,NULL,0,&kev,1,NULL);
    if (nready<0) {
      printf("kevent error: %s. kev.ident=%lu, kq is %d\n",strerror(errno),kev.ident,kq);
      exit(-1);
    }
    int fd = kev.ident;
    if (kev.flags & EV_EOF) {
      $str msg = $Plus$str$witness->$class->__add__($Plus$str$witness,$getName(fd),to$str(" closed connection\n"));
      if (fd_data[fd].errhandler)
        fd_data[fd].errhandler->$class ->__call__(fd_data[fd].errhandler,msg);
      else {
        perror("Remote host closed connection");
        exit(-1);
      }
      EV_SET(&fd_data[fd].event_spec,fd,EVFILT_READ,EV_DISABLE,0,0,NULL);
      kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
    }
    if (kev.flags & EV_ERROR) { 
      fprintf(stderr, "EV_ERROR: %s\n", strerror(kev.data));
      exit(-1);
    }
    if (fd==9999)
      break;
    switch (fd_data[fd].kind) {
    case connecthandler:
      if (kev.filter==EVFILT_READ) { // we are a listener and someone tries to connect
        while ((fd2 = accept(fd, (struct sockaddr *)&fd_data[fd].sock_addr,&socklen)) != -1) {
          fcntl(fd2,F_SETFL,O_NONBLOCK);
          fd_data[fd2].kind = connecthandler;
          fd_data[fd2].chandler = fd_data[fd].chandler;
          fd_data[fd2].sock_addr = fd_data[fd].sock_addr;
          bzero(fd_data[fd2].buffer,SOCK_BUF_SIZE); 
          EV_SET(&fd_data[fd2].event_spec,fd2,EVFILT_READ,EV_ADD,0,0,NULL);
          kevent(kq,&fd_data[fd2].event_spec,1,NULL,0,NULL);
          kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
          setupConnection(fd2);
          printf("%s %s\n","Connection from",$getName(fd2)->str);
        }
      } else { // we are a client and a delayed connection attempt has succeeded
        setupConnection(fd);
      }
      break;
    case readhandler:  // data has arrived on fd to fd_data[fd].buffer
      if (fd_data[fd].event_spec.filter == EVFILT_READ) {
        count = read(fd,&fd_data[fd].buffer,SOCK_BUF_SIZE);
        if (count < SOCK_BUF_SIZE)
          fd_data[fd].buffer[count] = 0;
        fd_data[fd].rhandler->$class->__call__(fd_data[fd].rhandler,to$str(fd_data[fd].buffer));
      } else {
        fprintf(stderr,"internal error: readhandler/event filter mismatch on descriptor %d\n",fd);
        exit(-1);
      }
      break;
    case nohandler:
      fprintf(stderr,"internal error: no event handler on descriptor %d\n",fd);
      exit(-1);
    }
    pthread_mutex_lock(&sleep_lock);
    pthread_cond_signal(&work_to_do);
    pthread_mutex_unlock(&sleep_lock);

  } 
}
