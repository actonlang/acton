/* Streaming API for netstrings. */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "netstring.h"

/* Reads a netstring from a `buffer` of length `buffer_length`. Writes
   to `netstring_start` a pointer to the beginning of the string in
   the buffer, and to `netstring_length` the length of the
   string. Does not allocate any memory. If it reads successfully,
   then it returns 0. If there is an error, then the return value will
   be negative. The error values are:

   NETSTRING_ERROR_TOO_LONG      More than 999999999 bytes in a field
   NETSTRING_ERROR_NO_COLON      No colon was found after the number
   NETSTRING_ERROR_TOO_SHORT     Number of bytes greater than buffer length
   NETSTRING_ERROR_NO_COMMA      No comma was found at the end
   NETSTRING_ERROR_LEADING_ZERO  Leading zeros are not allowed
   NETSTRING_ERROR_NO_LENGTH     Length not given at start of netstring

   If you're sending messages with more than 999999999 bytes -- about
   2 GB -- then you probably should not be doing so in the form of a
   single netstring. This restriction is in place partially to protect
   from malicious or erroneous input, and partly to be compatible with
   D. J. Bernstein's reference implementation.

   Example:
      if (netstring_read(&buf, &buflen, &str, &len) < 0) failed();
 */
int netstring_read(char **pbuffer, size_t *pbuffer_length,
                   char **netstring_start, size_t *netstring_length) {
  int i;
  size_t len = 0;
  char *buffer = *pbuffer;
  size_t buffer_length = *pbuffer_length;

  /* Write default values for outputs */
  *netstring_start = NULL; *netstring_length = 0;

  /* Make sure buffer is big enough. Minimum size is 3. */
  if (buffer_length < 3) return NETSTRING_ERROR_TOO_SHORT;

  /* No leading zeros allowed! */
  if (buffer[0] == '0' && isdigit(buffer[1]))
    return NETSTRING_ERROR_LEADING_ZERO;

  /* The netstring must start with a number */
  if (!isdigit(buffer[0])) return NETSTRING_ERROR_NO_LENGTH;

  /* Read the number of bytes */
  for (i = 0; i < buffer_length && isdigit(buffer[i]); i++) {
    /* Error if more than 9 digits */
    if (i >= 9) return NETSTRING_ERROR_TOO_LONG;
    /* Accumulate each digit, assuming ASCII. */
    len = len*10 + (buffer[i] - '0');
  }

  /* Check buffer length once and for all. Specifically, we make sure
     that the buffer is longer than the number we've read, the length
     of the string itself, and the colon and comma. */
  if (i + len + 1 >= buffer_length) return NETSTRING_ERROR_TOO_SHORT;

  /* Read the colon */
  if (buffer[i++] != ':') return NETSTRING_ERROR_NO_COLON;
  
  /* Test for the trailing comma */
  if (buffer[i + len] != ',') return NETSTRING_ERROR_NO_COMMA;

  /* Set the return values */
  *netstring_start = &buffer[i];
  *netstring_length = len;
  *pbuffer = *netstring_start + len + 1;
  *pbuffer_length = buffer_length - (i + len + 1);

  return 0;
}

/* Retrieves the size of the concatenated netstrings */
int netstring_list_size(char *buffer, size_t size, size_t *ptotal) {
  char  *str, *base = buffer;
  size_t len,  remaining = size;
  int rc;

  while( remaining>0 && (rc=netstring_read(&base, &remaining, &str, &len))==0 ){
  }

  if( rc==NETSTRING_ERROR_NO_LENGTH || rc==NETSTRING_ERROR_TOO_SHORT ) rc = 0;
  *ptotal = size - remaining;
  return rc;
}

/* Retrieves the number of concatenated netstrings */
int netstring_list_count(char *buffer, size_t size, int *pcount) {
  char  *str, *base = buffer;
  size_t len,  remaining = size;
  int rc, count = 0;

  while( remaining>0 && (rc=netstring_read(&base, &remaining, &str, &len))==0 ){
    count++;
  }

  if( rc==NETSTRING_ERROR_NO_LENGTH || rc==NETSTRING_ERROR_TOO_SHORT ) rc = 0;
  *pcount = count;
  return rc;
}

/* count the number of digits (base 10) in a positive integer */
int numdigits(size_t len) {
  int n = 1;
  if ( len >= 100000000 ) { n += 8; len /= 100000000; }
  if ( len >= 10000     ) { n += 4; len /= 10000; }
  if ( len >= 100       ) { n += 2; len /= 100; }
  if ( len >= 10        ) { n += 1; }
  return n;
}

/* Return the length, in ASCII characters, of a netstring containing
   `data_length` bytes. */
size_t netstring_buffer_size(size_t data_length) {
  return (size_t)numdigits(data_length) + data_length + 2;
}

/* Allocate and create a netstring containing the first `len` bytes of
   `data`. This must be manually freed by the client. If `len` is 0
   then no data will be read from `data`, and it may be NULL.
   Returns the netstring size not including the null terminator */
size_t netstring_add_ex(char **netstring, char *data, size_t len) {
  size_t num_len, size_prev=0, size_next;
  char *ptr;

  if (netstring == 0 || (len > 0 && data == 0)) return 0;

  num_len = numdigits(len);
  size_next = num_len + len + 2;

  if (*netstring == 0) {
    ptr = malloc(size_next + 1);
    if (ptr == 0) return 0;
    *netstring = ptr;
  } else {
    size_prev = strlen(*netstring);
    ptr = realloc(*netstring, size_prev + size_next + 1);
    if (ptr == 0) return 0;
    *netstring = ptr;
    ptr += size_prev;
  }

  if (len == 0) {
    strcpy(ptr, "0:,");
  } else {
    sprintf(ptr, "%lu:", (unsigned long)len);
    ptr += num_len + 1;
    memcpy(ptr, data, len);
    ptr += len; *ptr = ',';
    ptr++; *ptr = 0;
  }
  return size_prev + size_next;
}

size_t netstring_add(char **netstring, char *data) {
  return netstring_add_ex(netstring, data, strlen(data));
}
