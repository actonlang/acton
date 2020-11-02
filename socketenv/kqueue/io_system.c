#include "io_system.h"

#define BUF_SIZE 1024

#define MAX_FD  10

typedef enum HandlerCase {nohandler, readhandler, connecthandler} HandlerCase;

struct FileDescriptorData {
  HandlerCase kind;
  $Rd_Handler rhandler;
  $Rd_Handler errhandler;
  $conhandler chandler;
  struct sockaddr_in sock_addr;
  struct kevent event_spec;
  char buffer[BUF_SIZE];
};

struct FileDescriptorData fd_data[MAX_FD];

static void $init_FileDescriptorData(int fd) {
  fd_data[fd].kind = nohandler;
  bzero(fd_data[fd].buffer,BUF_SIZE);
}
int new_socket ($conhandler handler) {
  int fd = socket(PF_INET,SOCK_STREAM,0);
  //fcntl(fd,F_SETFL,O_NONBLOCK);
  fd_data[fd].kind = connecthandler;
  fd_data[fd].chandler = handler;
  return fd;
}  
void setupConnection (int fd,$str remoteHost) {
  $Connection conn = $NEW($Connection,fd,remoteHost);
  fd_data[fd].chandler(conn);
}

struct FileDescriptorData fd_data[MAX_FD];

// $Env /////////////////////////////////////////////////////////////////////////////

 
void $Env$__init__ ($Env self, int kq){
  self->kq = kq;
}

void $Env$stdout_write($Env self, $str str) {
  printf("%s",(char *)str->str);
}

void $Env$stdin_install($Env self,$Rd_Handler handler) {
  fd_data[STDIN_FILENO].kind = readhandler;
  fd_data[STDIN_FILENO].rhandler = handler;
}

void $Env$connect($Env self,$str remoteHost,$int remotePort,$conhandler handler) {
  struct sockaddr_in addr;
  struct in_addr iaddr;
  struct hostent *ent;
  int hostid;
  int fd = new_socket(handler);
  
  ent = gethostbyname((char *)remoteHost->str); //this should be replaced by calling getaddrinfo
  if(ent==NULL) {
    perror("Name lookup error");  // should connect have one more param prescribing what do with errors before connection is established?
  }
  else {
    memcpy(&hostid, ent->h_addr_list[0], sizeof hostid);
    iaddr.s_addr = hostid;
    fd_data[fd].sock_addr.sin_addr = iaddr;
    fd_data[fd].sock_addr.sin_port = htons(remotePort->val);
    fd_data[fd].sock_addr.sin_family = AF_INET;
    if (connect(fd,(struct sockaddr *)&fd_data[fd].sock_addr,sizeof(struct sockaddr)) < 0) {// couldn't connect immediately, 
      if (errno!=EINPROGRESS)  {                                                            // so check if attempt continues asynchronously.
        perror("connect failed");
        exit(-1);
      } else {                                              
        EV_SET(&fd_data[fd].event_spec,fd,EVFILT_WRITE,EV_ADD | EV_ONESHOT,0,0,NULL);
        kevent(self->kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
      }
    } else {
      setupConnection(fd,remoteHost);
      EV_SET(&fd_data[fd].event_spec,fd,EVFILT_READ,EV_ADD,0,0,NULL);
      kevent(self->kq,&fd_data[fd].event_spec,1,NULL,0,NULL);
    }
  }
}
    
void $Env$listen($Env self,$int port,$conhandler handler) {
  struct sockaddr_in addr;
  int fd = new_socket(handler);
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(port->val);
  addr.sin_family = AF_INET;
  if (bind(fd,(struct sockaddr *)&addr,sizeof(struct sockaddr)) < 0) {
    perror("bind failed");
    exit(-1);
  }
  listen(fd,5);
  //  return $NEW($Closable,fd);
}

struct $Env$class $Env$methods = {$Env$__init__,$Env$stdout_write,$Env$stdin_install,$Env$connect,$Env$listen};


void $Connection$__init__($Connection self,int descriptor,$str remoteHost) {
  self->descriptor = descriptor;
  self->remoteHost = remoteHost;
}

void $Connection$close($Connection self){
  close(self->descriptor); 
  $init_FileDescriptorData(self->descriptor);
}

void $Connection$write($Connection self, $str str) {
  memcpy(fd_data[self->descriptor].buffer,str->str,str->nbytes+1);
  int chunk_size = str->nbytes > BUF_SIZE ? BUF_SIZE : str->nbytes; 
  int r = write(self->descriptor,fd_data[self->descriptor].buffer,chunk_size);
  //  for now, assume str->nbytes < BUF_SIZE
}

void $Connection$on_receipt($Connection self,$Rd_Handler inputhandler,$Rd_Handler errhandler) {
  fd_data[self->descriptor].kind = readhandler;
  fd_data[self->descriptor].rhandler = inputhandler;
  fd_data[self->descriptor].errhandler = errhandler;
}

struct $Connection$class $Connection$methods = {$Connection$__init__,$Connection$close,$Connection$write,$Connection$on_receipt};

void event_loop(int kq) {
  EV_SET(&fd_data[STDIN_FILENO].event_spec,STDIN_FILENO,EVFILT_READ,EV_ADD,0,0,NULL);
  kevent(kq,&fd_data[STDIN_FILENO].event_spec,1,NULL,0,NULL);
  while(1) {
    struct kevent kev;
    struct sockaddr_in addr;
    socklen_t socklen = sizeof(addr);
    int fd2;
    int count;
    int nready = kevent(kq,NULL,0,&kev,1,NULL);
    int fd = kev.ident;
    if (kev.flags & EV_EOF) {
      printf("Discovered EV_EOF\n");
      if (fd_data[fd].errhandler)
        fd_data[fd].errhandler->$handle(fd_data[fd].errhandler->obj,to$str("Remote host closed connection"));
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
    switch (fd_data[fd].kind) {
    case connecthandler:
      while ((fd2 = accept(kq, (struct sockaddr *)&addr,&socklen)) != -1) {
        fcntl(fd2,F_SETFL,O_NONBLOCK);
        fd_data[fd2].sock_addr = addr;
        bzero(fd_data[fd2].buffer,BUF_SIZE); 
        EV_SET(&fd_data[fd2].event_spec,fd2,EVFILT_READ,EV_ADD,0,0,NULL);
        kevent(kq,&fd_data[fd2].event_spec,1,NULL,0,NULL);
        char buf[100];
        getnameinfo((struct sockaddr *)&addr,socklen,buf,100,NULL,0,0);
        setupConnection(fd2,to$str(buf));
      }
      break;
    case readhandler:  // data has arrived on fd to fd_data[fd].buffer
      if (fd_data[fd].event_spec.filter == EVFILT_READ) {
        count = read(fd,&fd_data[fd].buffer,BUF_SIZE);
        if (count < BUF_SIZE)
          fd_data[fd].buffer[count] = 0;
        fd_data[fd].rhandler->$handle(fd_data[fd].rhandler->obj,to$str(fd_data[fd].buffer));
      } else {
        fprintf(stderr,"internal error: readhandler/event filter mismatch on descriptor %d\n",fd);
        exit(-1);
      }
      break;
    case nohandler:
      fprintf(stderr,"internal error: no event handler on descriptor %d\n",fd);
        exit(-1);
    }
  }
}

/*

int selector_fd = kqueue()

handler[MAX_FD] = empty
event_spec[MAX_FD] = empty
sock_addr[MAX_FD] = empty
data_buffer[MAX_FD] = empty

for each thread:   //???
    while (1):
        event_t event
        int nready = kevent_WAIT(selector_fd, &event, 1)
        int fd = (int)event.ident
        handler[fd](fd):
            handle_listen(fd):
                sockaddr
                while (int fd2 = accept(fd, &sockaddr)): // should we require that result be not -1?
                    set_non_blocking(fd2)
                    try:
                        handler[fd2] = handle_connect
                        sock_addr[fd2] = sockaddr
                        bzero(&data_buffer[fd2])
                        EV_SET(event_spec[fd2], fd2, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL)
                        kevent_CHANGE(selector_fd, &event_spec[fd2], 1)
                    catch:
                        event_spec[fd2].flags = EV_DELETE
                        kevent_CHANGE(selector_fd, &event_spec[fd2], 1)
            handle_connect(fd):
                if event_spec[fd].filter == EVFILT_READ:
                    count = read(&data_buffer[fd])
                elif event_spec[fd].filter == EVFILT_WRITE:
                    count = write(&data_buffer[fd]);



to listen:
    try:
        fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        handler[fd] = handle_listen
        sock_addr[fd] = { AF_INET, address, htons(port) }
        bind(fd, sock_addr[fd])
        listen(fd, 65535)
        EV_SET(event_spec[fd], fd, EVFILT_READ, EV_ADD, 0, 0, NULL)
        kevent_CHANGE(selector_fd, &event_spec[fd], 1)
    catch:
        event_spec[fd].flags = EV_DELETE
        kevent_CHANGE(selector_fd, &event_spec[fd], 1)

    

*/

