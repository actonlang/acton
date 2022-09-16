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

////////////////////////////////////////////////////////////////////////////////////////

$bool $proc$__bool__($proc self) {
  return $True;
}
$str $proc$__str__($proc self) {
  char *s;
  asprintf(&s,"<proc closure at %p>",self);
  return to$str(s);
}

$bool $action$__bool__($action self) {
  return $True;
}
$str $action$__str__($action self) {
  char *s;
  asprintf(&s,"<action closure at %p>",self);
  return to$str(s);
}

$bool $mut$__bool__($mut self) {
  return $True;
}
$str $mut$__str__($mut self) {
  char *s;
  asprintf(&s,"<mut closure at %p>",self);
  return to$str(s);
}

$bool $pure$__bool__($pure self) {
  return $True;
}
$str $pure$__str__($pure self) {
  char *s;
  asprintf(&s,"<pure closure at %p>",self);
  return to$str(s);
}

void $Cont$__init__($Cont $this) {
    // Empty
}
$bool $Cont$__bool__($Cont self) {
  return $True;
}
$str $Cont$__str__($Cont self) {
  char *s;
  asprintf(&s,"<$Cont closure at %p>",self);
  return to$str(s);
}
void $Cont$__serialize__($Cont self, $Serial$state state) {
    // Empty
}
$Cont $Cont$__deserialize__($Cont self, $Serial$state state) {
    return $DNEW($Cont,state);
}

struct $proc$class $proc$methods = {
    "$proc",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $proc$__bool__,
    $proc$__str__,
    $proc$__str__,
    NULL,               /* __eval__ */
    NULL                /* __exec__ */
};
struct $action$class $action$methods = {
    "$action",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $action$__bool__,
    $action$__str__,
    $action$__str__,
    NULL,               /* __eval__ */
    NULL,               /* __exec__ */
    NULL                /* __asyn__ */
};
struct $mut$class $mut$methods = {
    "$mut",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $mut$__bool__,
    $mut$__str__,
    $mut$__str__,
    NULL,               /* __eval__ */
    NULL,               /* __exec__ */
    NULL                /* __call__ */
};
struct $pure$class $pure$methods = {
    "$pure",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    $pure$__bool__,
    $pure$__str__,
    $pure$__str__,
    NULL,               /* __eval__ */
    NULL,               /* __exec__ */
    NULL                /* __call__ */
};

struct $Cont$class $Cont$methods = {
    "$Cont",
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    $Cont$__str__,
    NULL                /* __call__ */
};

///////////////////////////////////////////////////////

void $function$__init__($function $this) { }

$bool $function$__bool__($function self) {
  return $True;
}

$str $function$__str__($function self) {
  char *s;
  asprintf(&s,"<function object at %p>",self);
  return to$str(s);
}

void $function$__serialize__($function self, $Serial$state state) {
    // TBD
}

$function $function$__deserialize__($function self, $Serial$state state) {
    // TBD
    return NULL;
}

struct $function$class $function$methods = {
    "$function",
    UNASSIGNED,
    NULL,
    $function$__init__,
    $function$__serialize__,
    $function$__deserialize__,
    $function$__bool__,
    $function$__str__,
    $function$__str__,
    NULL
};
