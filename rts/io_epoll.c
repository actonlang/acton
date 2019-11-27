#include <sys/epoll.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * This program is an example for CS 241 @ UIUC
 * Edited by Austin Walters for austingwalters.com 
 * Program is a simple epoll example - only runs on linux
 */

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

    /**
    * Create the epoll() 
    *
    * From man pages
    * Since Linux 2.6.8, the
    * size argument is ignored, 
    * but must be greater than zero;
    */
    int epoll_fd = epoll_create(1);

    /* Double-array of fds for the pipe() */
    int fds[2][2];

    /* Each process needs its own epoll struct */
    struct epoll_event event[2];

    /* Create two processs... */
    int i;
    for (i = 0; i < 2; i++) {

        /* Create a pipe */
        fds[i] = malloc(2 * sizeof(int));
        pipe(fds[i]);

        int read_fd = fds[i][0];
        int write_fd = fds[i][1];

        /* Generates a new process */
        pid_t pid = fork();

        /* child */
        if (pid == 0) {

            close(read_fd);

            if (i == 0) { child_one_func(write_fd); }
            else if (i == 1) { child_two_func(write_fd); }

            /* Closes child */
            exit(0);

        } else {
            close(write_fd);
        }

        /* Setup the epoll_event for this process */
        event[i].events = EPOLLIN;

        /* Parent process listening to read file descriptor */
        event[i].data.fd = read_fd;

        /* Add Listening (Read) event on epoll_fds -- event[i] */
        epoll_ctl( epoll_fd, EPOLL_CTL_ADD, read_fd, &event[i]);
    }

    /**
    * Read data via epoll_wait() 
    *
    * It prints out the data as it comes in.
    */
    while (1) {

        struct epoll_event ev;
        epoll_wait(epoll_fd, &ev, 1, -1);

        char str[10];
        ssize_t bytes = read(ev.data.fd, &str, 10);
        str[bytes] = '\0';

        /* If the ev.data.fd has bytes added print, else wait */
        if (bytes > 0)
            printf("Read: %s\n", str);
        else
            epoll_ctl(epoll_fd, EPOLL_CTL_DEL, ev.data.fd, NULL);
        if (strcmp(str, "D - 4") == 0)
            return 0;
    }
}
