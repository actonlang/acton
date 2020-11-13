#include "minienv.h"



static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  bzero(fd_data[fd].buffer,BUF_SIZE);
}

int new_socket ($function handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}

void setupConnection (int fd, $str remoteHost) {
  printf("setting up connection\n");
  minienv$$Connection conn = $NEW(minienv$$Connection,fd,NULL);
  fd_data[fd].chandler->$class->__call__(fd_data[fd].chandler, conn); // ???????
}


void* minienv$$l$1lambda$__init__ (minienv$$l$1lambda l$self, minienv$$Env __self__, $str s) {
    l$self->__self__ = __self__;
    l$self->s = s;
    return NULL;
}
$R minienv$$l$1lambda$__call__ (minienv$$l$1lambda l$self, $Cont c$cont) {
    minienv$$Env __self__ = l$self->__self__;
    $str s = l$self->s;
    return $APP(minienv$$Env, __self__, stdout_write$local, s, c$cont);
}
struct minienv$$l$1lambda$class minienv$$l$1lambda$methods;
void* minienv$$l$2lambda$__init__ (minienv$$l$2lambda l$self, minienv$$Env __self__, $function cb) {
    l$self->__self__ = __self__;
    l$self->cb = cb;
    return NULL;
}
$R minienv$$l$2lambda$__call__ (minienv$$l$2lambda l$self, $Cont c$cont) {
    minienv$$Env __self__ = l$self->__self__;
    $function cb = l$self->cb;
    return $APP(minienv$$Env, __self__, stdin_install$local, cb, c$cont);
}
struct minienv$$l$2lambda$class minienv$$l$2lambda$methods;
void* minienv$$l$3lambda$__init__ (minienv$$l$3lambda l$self, minienv$$Env __self__, $str host, $int port, $function cb) {
    l$self->__self__ = __self__;
    l$self->host = host;
    l$self->port = port;
    l$self->cb = cb;
    return NULL;
}
$R minienv$$l$3lambda$__call__ (minienv$$l$3lambda l$self, $Cont c$cont) {
    minienv$$Env __self__ = l$self->__self__;
    $str host = l$self->host;
    $int port = l$self->port;
    $function cb = l$self->cb;
    return $APP(minienv$$Env, __self__, connect$local, host, port, cb, c$cont);
}
struct minienv$$l$3lambda$class minienv$$l$3lambda$methods;
void* minienv$$l$4lambda$__init__ (minienv$$l$4lambda l$self, minienv$$Env __self__, $int port, $function cb) {
    l$self->__self__ = __self__;
    l$self->port = port;
    l$self->cb = cb;
    return NULL;
}
$R minienv$$l$4lambda$__call__ (minienv$$l$4lambda l$self, $Cont c$cont) {
    minienv$$Env __self__ = l$self->__self__;
    $int port = l$self->port;
    $function cb = l$self->cb;
    return $APP(minienv$$Env, __self__, listen$local, port, cb, c$cont);
}
struct minienv$$l$4lambda$class minienv$$l$4lambda$methods;
void* minienv$$l$5lambda$__init__ (minienv$$l$5lambda l$self, minienv$$Connection __self__, $str s) {
    l$self->__self__ = __self__;
    l$self->s = s;
    return NULL;
}
$R minienv$$l$5lambda$__call__ (minienv$$l$5lambda l$self, $Cont c$cont) {
    minienv$$Connection __self__ = l$self->__self__;
    $str s = l$self->s;
    return $APP(minienv$$Connection, __self__, write$local, s, c$cont);
}
struct minienv$$l$5lambda$class minienv$$l$5lambda$methods;
void* minienv$$l$6lambda$__init__ (minienv$$l$6lambda l$self, minienv$$Connection __self__) {
    l$self->__self__ = __self__;
    return NULL;
}
$R minienv$$l$6lambda$__call__ (minienv$$l$6lambda l$self, $Cont c$cont) {
       minienv$$Connection __self__ = l$self->__self__;
    return $APP(minienv$$Connection, __self__, close$local, c$cont);
}
struct minienv$$l$6lambda$class minienv$$l$6lambda$methods;
void* minienv$$l$7lambda$__init__ (minienv$$l$7lambda l$self, minienv$$Connection __self__, $function cb1, $function cb2) {
    l$self->__self__ = __self__;
    l$self->cb1 = cb1;
    l$self->cb2 = cb2;
    return NULL;
}
$R minienv$$l$7lambda$__call__ (minienv$$l$7lambda l$self, $Cont c$cont) {
    minienv$$Connection __self__ = l$self->__self__;
    $function cb1 = l$self->cb1;
    $function cb2 = l$self->cb2;
    return $APP(minienv$$Connection, __self__, on_receipt$local, cb1, cb2, c$cont);
}
struct minienv$$l$7lambda$class minienv$$l$7lambda$methods;
$R minienv$$Env$__init__ (minienv$$Env __self__, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
$R minienv$$Env$stdout_write$local (minienv$$Env __self__, $str s, $Cont c$cont) {
    printf("%s",s->str);
    return $R_CONT(c$cont, $None);
}
$R minienv$$Env$stdin_install$local (minienv$$Env __self__, $function cb, $Cont c$cont) {
    fd_data[STDIN_FILENO].kind = readhandler;
    fd_data[STDIN_FILENO].rhandler = cb;
    EV_SET(&fd_data[STDIN_FILENO].event_spec,STDIN_FILENO,EVFILT_READ,EV_ADD,0,0,NULL);
    kevent(kq,&fd_data[STDIN_FILENO].event_spec,1,NULL,0,NULL);
    return $R_CONT(c$cont, $None);
}
$R minienv$$Env$connect$local (minienv$$Env __self__, $str host, $int port, $function cb, $Cont c$cont) {
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
          printf("connect returned EINPROGRESS error on fd %d\n",fd);
          EV_SET(&fd_data[fd].event_spec,fd,EVFILT_WRITE,EV_ADD | EV_ONESHOT,0,0,NULL);
          kevent(kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
        } else {
          fprintf(stderr,"Connect failed");
          exit(-1);
          //netError(fd,"Connect failed");
        }
      } else // connect succeeded immediately (can this ever happen?)
        setupConnection(fd,host);
    }
    return $R_CONT(c$cont, $None);
}
$R minienv$$Env$listen$local (minienv$$Env __self__, $int port, $function cb, $Cont c$cont) {
    struct sockaddr_in addr;
    int fd = new_socket(cb);
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port->val);
    addr.sin_family = AF_INET;
    if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0)
      perror("bind failed");
    listen(fd,5);
    return $R_CONT(c$cont, $None);
}
$Msg minienv$$Env$stdout_write (minienv$$Env __self__, $str s) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$1lambda, __self__, s));
}
$Msg minienv$$Env$stdin_install (minienv$$Env __self__, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$2lambda, __self__, cb));
}
$Msg minienv$$Env$connect (minienv$$Env __self__, $str host, $int port, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$3lambda, __self__, host, port, cb));
}
$Msg minienv$$Env$listen (minienv$$Env __self__, $int port, $function cb) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$4lambda, __self__, port, cb));
}
struct minienv$$Env$class minienv$$Env$methods;
$R minienv$$Connection$__init__ (minienv$$Connection __self__, int descriptor, $Cont c$cont) {
    __self__->descriptor = descriptor;
    return $R_CONT(c$cont, $None);
}
$R minienv$$Connection$write$local (minienv$$Connection __self__, $str s, $Cont c$cont) {
    memcpy(fd_data[__self__->descriptor].buffer,s->str,s->nbytes+1);
    int chunk_size = s->nbytes > BUF_SIZE ? BUF_SIZE : s->nbytes; 
    int r = write(__self__->descriptor,fd_data[__self__->descriptor].buffer,chunk_size);
    //  for now, assume str->nbytes < BUF_SIZE
    return $R_CONT(c$cont, $None);
}
$R minienv$$Connection$close$local (minienv$$Connection __self__, $Cont c$cont) {
    close(__self__->descriptor); 
    $init_FileDescriptorData(__self__->descriptor);
    return $R_CONT(c$cont, $None);
}
$R minienv$$Connection$on_receipt$local (minienv$$Connection __self__, $function on_input, $function on_error, $Cont c$cont) {
    fd_data[__self__->descriptor].kind = readhandler;
    fd_data[__self__->descriptor].rhandler = on_input;
    fd_data[__self__->descriptor].errhandler = on_error;
    return $R_CONT(c$cont, $None);
}
$Msg minienv$$Connection$write (minienv$$Connection __self__, $str s) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$5lambda, __self__, s));
}
$Msg minienv$$Connection$close (minienv$$Connection __self__) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$6lambda, __self__));
}
$Msg minienv$$Connection$on_receipt (minienv$$Connection __self__, $function cb1, $function cb2) {
    return $ASYNC(($Actor)__self__, ($Cont)$NEW(minienv$$l$7lambda, __self__, cb1, cb2));
}
struct minienv$$Connection$class minienv$$Connection$methods;
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
        minienv$$Env$methods.$GCINFO = "minienv$$Env";
        minienv$$Env$methods.$superclass = ($Super$class)&$Actor$methods;
        minienv$$Env$methods.__bool__ = ($bool (*)(minienv$$Env))$Actor$methods.__bool__;
        minienv$$Env$methods.__str__ = ($str (*)(minienv$$Env))$Actor$methods.__str__;
        minienv$$Env$methods.__init__ = minienv$$Env$__init__;
        minienv$$Env$methods.stdout_write$local = minienv$$Env$stdout_write$local;
        minienv$$Env$methods.stdin_install$local = minienv$$Env$stdin_install$local;
        minienv$$Env$methods.connect$local = minienv$$Env$connect$local;
        minienv$$Env$methods.listen$local = minienv$$Env$listen$local;
        minienv$$Env$methods.stdout_write = minienv$$Env$stdout_write;
        minienv$$Env$methods.stdin_install = minienv$$Env$stdin_install;
        minienv$$Env$methods.connect = minienv$$Env$connect;
        minienv$$Env$methods.listen = minienv$$Env$listen;
        $register(&minienv$$Env$methods);
    }
    {
        minienv$$Connection$methods.$GCINFO = "minienv$$Connection";
        minienv$$Connection$methods.$superclass = ($Super$class)&$Actor$methods;
        minienv$$Connection$methods.__bool__ = ($bool (*)(minienv$$Connection))$Actor$methods.__bool__;
        minienv$$Connection$methods.__str__ =  ($str (*)(minienv$$Connection))$Actor$methods.__str__;
        minienv$$Connection$methods.__init__ = minienv$$Connection$__init__;
        minienv$$Connection$methods.write$local = minienv$$Connection$write$local;
        minienv$$Connection$methods.close$local = minienv$$Connection$close$local;
        minienv$$Connection$methods.on_receipt$local = minienv$$Connection$on_receipt$local;
        minienv$$Connection$methods.write = minienv$$Connection$write;
        minienv$$Connection$methods.close = minienv$$Connection$close;
        minienv$$Connection$methods.on_receipt = minienv$$Connection$on_receipt;
        $register(&minienv$$Connection$methods);
    }
}
