          while(1) {
            struct kevent timer;
            EV_SET(&timer, 9999, EVFILT_TIMER, EV_ADD | EV_ONESHOT, 0, 500, 0);
            kevent(kq,&timer,1,0,0,0);
            struct kevent kev;
            struct sockaddr_in addr;
            socklen_t socklen = sizeof(addr);
            int fd2;
            int count;
            int nready = kevent(kq,NULL,0,&kev,1,NULL);
            if (nready<0) {
              printf("kevent error %s. kev.ident=%lu, kq is %d\n",strerror(errno),kev.ident,kq);
               exit(-1);
            }
            int fd = kev.ident;
            if (kev.flags & EV_EOF) {
              printf("Discovered EV_EOF\n");
              if (fd_data[fd].errhandler)
                fd_data[fd].errhandler->$class ->__call__(fd_data[fd].errhandler,to$str("Remote host closed connection"));
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
              printf("connect event on descriptor %d\n",fd);
              if (kev.filter==EVFILT_READ) { // we are a listener and someone tries to connect
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
              }else { // we are a client and a delayed connection attempt has succeeded
                setupConnection(fd,to$str("TODO Fix host name here"));
              }
              break;
            case readhandler:  // data has arrived on fd to fd_data[fd].buffer
              if (fd_data[fd].event_spec.filter == EVFILT_READ) {
                count = read(fd,&fd_data[fd].buffer,BUF_SIZE);
                if (count < BUF_SIZE)
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
          } 
