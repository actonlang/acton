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

/*
B_bool $procD___bool__($proc self) {
  return B_True;
}
B_str $procD___str__($proc self) {
  return $FORMAT("<proc closure at %p>", self);
}

B_bool $actionD___bool__($action self) {
  return B_True;
}
B_str $actionD___str__($action self) {
  return $FORMAT("<action closure at %p>", self);
}

B_bool $mutD___bool__($mut self) {
  return B_True;
}
B_str $mutD___str__($mut self) {
  return $FORMAT("<mut closure at %p>", self);
}

B_bool $pureD___bool__($pure self) {
  return B_True;
}
B_str $pureD___str__($pure self) {
  return $FORMAT("<pure closure at %p>", self);
}
*/
void $ContD___init__($Cont $this) {
    // Empty
}
/*
B_bool $ContD___bool__($Cont self) {
  return B_True;
}
B_str $ContD___str__($Cont self) {
  return $FORMAT("<$Cont closure at %p>", self);
}
*/
void $ContD___serialize__($Cont self, $Serial$state state) {
    // Empty
}
$Cont $ContD___deserialize__($Cont self, $Serial$state state) {
    return $DNEW($Cont,state);
}

struct $ContG_class $ContG_methods = {
    "$Cont",
    UNASSIGNED,
    NULL,
    $ContD___init__,
    $ContD___serialize__,
    $ContD___deserialize__,
    NULL                /* __call__ */
};
struct $procG_class $procG_methods = {
    "$proc",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    NULL,               /* __call__ */
    NULL                /* __exec__ */
};
struct $actionG_class $actionG_methods = {
    "$action",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __asyn__ */
};
struct $mutG_class $mutG_methods = {
    "$mut",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __eval__ */
};
struct $pureG_class $pureG_methods = {
    "$pure",
    UNASSIGNED,
    NULL,
    NULL,               /* __init__ */
    NULL,               /* __serialize__ */
    NULL,               /* __deserialize__ */
    NULL,               /* __call__ */
    NULL,               /* __exec__ */
    NULL                /* __eval__ */
};
