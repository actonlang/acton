/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "time/time.h"
#include <time.h>


$NoneType time$$RealFuns$__init__ (time$$RealFuns w$self) {
    return $None;
}
time$$RealFuns time$$RealFuns$new() {
    time$$RealFuns $tmp = malloc(sizeof(struct time$$RealFuns));
    $tmp->$class = &time$$RealFuns$methods;
    time$$RealFuns$methods.__init__($tmp);
    return $tmp;
}
struct time$$RealFuns$class time$$RealFuns$methods;
$NoneType time$$RealFuns$float$__init__ (time$$RealFuns$float w$self) {
    time$$RealFuns$methods.__init__((time$$RealFuns)w$self);
    return $None;
}

$NoneType time$$RealFuns$float$__serialize__(time$$RealFuns$float wit, $Serial$state state) {
    return $None;
}

time$$RealFuns$float time$$RealFuns$float$__deserialize__(time$$RealFuns$float wit, $Serial$state state) {
    time$$RealFuns$float res = $DNEW(time$$RealFuns$float,state);
    return res;
}
$str time$$RealFuns$float$time(time$$RealFuns$float wit, $float x) {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
    perror("clock_gettime");
    exit(EXIT_FAILURE);
  }
  return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}

time$$RealFuns$float time$$RealFuns$float$new() {
    time$$RealFuns$float $tmp = malloc(sizeof(struct time$$RealFuns$float));
    $tmp->$class = &time$$RealFuns$float$methods;
    time$$RealFuns$float$methods.__init__($tmp);
    return $tmp;
}
struct time$$RealFuns$float$class time$$RealFuns$float$methods;
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
    {
        time$$RealFuns$methods.$GCINFO = "time$$RealFuns";
        time$$RealFuns$methods.$superclass = NULL;
        time$$RealFuns$methods.__init__ = time$$RealFuns$__init__;
        $register(&time$$RealFuns$methods);
    }
    {
        time$$RealFuns$float$methods.$GCINFO = "time$$RealFuns$float";
        time$$RealFuns$float$methods.$superclass = ($Super$class)&time$$RealFuns$methods;
        time$$RealFuns$float$methods.__serialize__ = time$$RealFuns$float$__serialize__,
        time$$RealFuns$float$methods.__deserialize__ = time$$RealFuns$float$__deserialize__,
        time$$RealFuns$float$methods.__bool__ = ($bool (*)(time$$RealFuns$float))$default__bool__,
        time$$RealFuns$float$methods.__str__ = ($str (*)(time$$RealFuns$float))$default__str__,
        time$$RealFuns$float$methods.__init__ = time$$RealFuns$float$__init__;
        time$$RealFuns$float$methods.time = time$$RealFuns$float$time;
        $register(&time$$RealFuns$float$methods);
    }
}

$WORD time$$time (time$$RealFuns wit, $WORD x) {
  return wit->$class->time(wit,x);
}
