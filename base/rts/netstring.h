#ifndef __NETSTRING_STREAM_H
#define __NETSTRING_STREAM_H

#include <string.h>

size_t netstring_add(char **netstring, char *data);
size_t netstring_add_ex(char **netstring, char *data, size_t len);

int netstring_read(char **buffer_start, size_t *buffer_length,
                   char **netstring_start, size_t *netstring_length);

size_t netstring_buffer_size(size_t data_length);

int netstring_list_size(char *buffer, size_t size, size_t *ptotal);
int netstring_list_count(char *buffer, size_t size, int *pcount);

/* Errors that can occur during netstring parsing */
#define NETSTRING_ERROR_TOO_LONG     -1
#define NETSTRING_ERROR_NO_COLON     -2
#define NETSTRING_ERROR_TOO_SHORT    -3
#define NETSTRING_ERROR_NO_COMMA     -4
#define NETSTRING_ERROR_LEADING_ZERO -5
#define NETSTRING_ERROR_NO_LENGTH    -6

#endif
