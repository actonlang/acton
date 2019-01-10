/**
 * Written by Austin Walters 5/13/2014
 * For I/O Multiplexing example on austingwalters.com
 */

#include <sys/event.h>
#include <sys/time.h> 
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h> 
#include <unistd.h>
#include <string.h>

/**
 * Given the file descriptor this function,
 * writes "A", waits 2 seconds, writes "C"
 */
void child_one_func(int fd) {

    write(fd, "A - 1", 5);
    sleep(2);
    write(fd, "C - 3", 5);
    close(fd);
}

/**
 * Given the file descriptor this function,
 * Waits 1 second, writes "B", Waits 2 seconds,
 * then writes "D"
 */
void child_two_func(int fd) {

    sleep(1);
    write(fd, "B - 2", 5);
    sleep(2);
    write(fd, "D - 4", 5);
    close(fd);
}

int main() {

    int kq = kqueue();
  
    /* Double-array of fds for the pipe() */
    int fds[2][2];
    struct kevent ev;

    int i;
    for (i = 0; i < 2; i++) {

        /* Create a pipe */
        pipe(fds[i]);

        int read_fd = fds[i][0];
        int write_fd = fds[i][1];
    
        /* Generates a new process */
        pid_t pid = fork();

        /* child */
        if (pid == 0){

            close(read_fd);

            if (i == 0) { child_one_func(write_fd); }
            else if (i == 1) { child_two_func(write_fd); }

            /* Closes child */
            exit(0);

        } else {
            close(write_fd);
        }

        /* Set listener to read */
        EV_SET(&ev, read_fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
        kevent(kq, &ev, 1, NULL, 0, NULL);
    }

    
    while (1) {

        /* Grab any event */
        kevent(kq, NULL, 0, &ev, 1, NULL);

        char str[10];
        ssize_t bytes = read(ev.ident, &str, 10);
        str[bytes] = '\0';

        if (bytes > 0)
            printf("Read: %s\nFlags: %X\n", str, ev.flags);
        if (strcmp(str, "D - 4") == 0)
            return 0;
    }
}
