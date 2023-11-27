/*
 * Copyright (c) 2020 rxi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifdef ACTON_THREADS
#include <pthread.h>
#endif

#include <uv.h>
#include "log.h"

#define MAX_CALLBACKS 32
#ifdef ACTON_THREADS
static pthread_mutex_t l_mutex;
#endif

typedef struct {
  log_LogFn fn;
  void *udata;
  int level;
} Callback;

static struct {
  void *udata;
  int level;
  bool quiet;
  Callback callbacks[MAX_CALLBACKS];
} L;


static const char *level_strings[] = {
  "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

#ifdef LOG_USE_COLOR
static const char *level_colors[] = {
  "\x1b[94m", "\x1b[36m", "\x1b[32m", "\x1b[33m", "\x1b[31m", "\x1b[35m"
};
#endif


static void stdout_callback(log_Event *ev) {
  // Get a short thread name
  char tname[16] = "";
#ifdef ACTON_THREADS
  pthread_getname_np(pthread_self(), tname, 16);
#endif
  if (strncmp(tname, "IO", 6) == 0) {
  } else if (strncmp(tname, "Worker", 6) == 0) {
    strncpy(tname, &tname[7], 4);
  } else {
    // main thread, ignore the name
    strcpy(tname, "");
  }

  char buf[16];
  buf[strftime(buf, sizeof(buf), "%H:%M:%S", ev->date)] = '\0';
#ifdef LOG_USE_COLOR
  fprintf(
    ev->udata, "%s.%06lu %s%-5s\x1b[0m \x1b[90m%-20s:%5d:\x1b[0m RTS %2s: ",
    buf, ev->ts.tv_nsec/1000, level_colors[ev->level], level_strings[ev->level],
    ev->file, ev->line, tname);
#else
  fprintf(
    ev->udata, "%s.%06lu %-5s %-20s:%5d: RTS %2s: ",
    buf, ev->ts.tv_nsec/1000, level_strings[ev->level], ev->file, ev->line, tname);
#endif
  vfprintf(ev->udata, ev->fmt, ev->ap);
  fprintf(ev->udata, "\n");
  fflush(ev->udata);
}


static void file_callback(log_Event *ev) {
  // Get a short thread name
  char tname[16];
#ifdef ACTON_THREADS
  pthread_getname_np(pthread_self(), tname, 16);
#endif
  if (strncmp(tname, "IO", 6) == 0) {
  } else if (strncmp(tname, "Worker", 6) == 0) {
    strncpy(tname, &tname[7], 4);
  } else {
    // main thread, ignore the name
    strcpy(tname, "");
  }

  char buf[64];
  buf[strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", ev->date)] = '\0';
  fprintf(
    ev->udata, "%s.%09lu %-5s %-20s:%5d: RTS %2s: ",
    buf, ev->ts.tv_nsec, level_strings[ev->level], ev->file, ev->line, tname);
  vfprintf(ev->udata, ev->fmt, ev->ap);
  fprintf(ev->udata, "\n");
  fflush(ev->udata);
}


static void lock(void)   {
#ifdef ACTON_THREADS
  pthread_mutex_lock(&l_mutex);
#endif
}


static void unlock(void) {
#ifdef ACTON_THREADS
  pthread_mutex_unlock(&l_mutex);
#endif
}


const char* log_level_string(int level) {
  return level_strings[level];
}

int log_get_level() {
  return L.level;
}

void log_set_level(int level) {
  L.level = level;
}


void log_set_quiet(bool enable) {
  L.quiet = enable;
}


int log_add_callback(log_LogFn fn, void *udata, int level) {
  for (int i = 0; i < MAX_CALLBACKS; i++) {
    if (!L.callbacks[i].fn) {
      L.callbacks[i] = (Callback) { fn, udata, level };
      return 0;
    }
  }
  return -1;
}


int log_add_fp(FILE *fp, int level) {
  return log_add_callback(file_callback, fp, level);
}


static void init_event(log_Event *ev, void *udata) {
  if (!ev->date) {
    uv_clock_gettime(UV_CLOCK_REALTIME, &ev->ts);
    ev->date = localtime(&ev->ts.tv_sec);
  }
  ev->udata = udata;
}


void log_log(int level, const char *file, int line, const char *fmt, ...) {
  log_Event ev = {
    .fmt   = fmt,
    .file  = file,
    .line  = line,
    .level = level,
  };

  lock();

  if (!L.quiet && level >= L.level) {
    init_event(&ev, stderr);
    va_start(ev.ap, fmt);
    stdout_callback(&ev);
    va_end(ev.ap);
  }

  for (int i = 0; i < MAX_CALLBACKS && L.callbacks[i].fn; i++) {
    Callback *cb = &L.callbacks[i];
    if (level >= cb->level) {
      init_event(&ev, cb->udata);
      va_start(ev.ap, fmt);
      cb->fn(&ev);
      va_end(ev.ap);
    }
  }

  unlock();
}
