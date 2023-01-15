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

B_BaseException B_BaseExceptionG_new(B_str error_message) {
  return $NEW(B_BaseException, error_message);
}

void B_BaseExceptionD___init__(B_BaseException self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_BaseExceptionD___bool__(B_BaseException self) {
  return B_True;
}

B_str B_BaseExceptionD___str__(B_BaseException self) {
  char *s;
  asprintf(&s,"BaseException:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_BaseExceptionD___serialize__(B_BaseException self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_BaseException B_BaseExceptionD___deserialize__(B_BaseException self, $Serial$state state) {
  B_BaseException res = $DNEW(B_BaseException,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_BaseExceptionG_class B_BaseExceptionG_methods = {"B_BaseException",UNASSIGNED,NULL,B_BaseExceptionD___init__,
                                                      B_BaseExceptionD___serialize__,B_BaseExceptionD___deserialize__,B_BaseExceptionD___bool__,
                                                      B_BaseExceptionD___str__,B_BaseExceptionD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_SystemExit B_SystemExitG_new(B_str error_message) {
  return $NEW(B_SystemExit, error_message);
}


void B_SystemExitD___init__(B_SystemExit self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_SystemExitD___bool__(B_SystemExit self) {
  return B_True;
}

B_str B_SystemExitD___str__(B_SystemExit self) {
  char *s;
  asprintf(&s,"SystemExit:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_SystemExitD___serialize__(B_SystemExit self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_SystemExit B_SystemExitD___deserialize__(B_SystemExit self, $Serial$state state) {
  B_SystemExit res = $DNEW(B_SystemExit,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_SystemExitG_class B_SystemExitG_methods = {"B_SystemExit",UNASSIGNED,($SuperG_class)&B_BaseExceptionG_methods,B_SystemExitD___init__,
                                               B_SystemExitD___serialize__,B_SystemExitD___deserialize__,B_SystemExitD___bool__,
                                               B_SystemExitD___str__,B_SystemExitD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_KeyboardInterrupt B_KeyboardInterruptG_new(B_str error_message) {
  return $NEW(B_KeyboardInterrupt, error_message);
}

void B_KeyboardInterruptD___init__(B_KeyboardInterrupt self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_KeyboardInterruptD___bool__(B_KeyboardInterrupt self) {
  return B_True;
}

B_str B_KeyboardInterruptD___str__(B_KeyboardInterrupt self) {
  char *s;
  asprintf(&s,"KeyboardInterrupt:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_KeyboardInterruptD___serialize__(B_KeyboardInterrupt self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_KeyboardInterrupt B_KeyboardInterruptD___deserialize__(B_KeyboardInterrupt self, $Serial$state state) {
  B_KeyboardInterrupt res = $DNEW(B_KeyboardInterrupt,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_KeyboardInterruptG_class B_KeyboardInterruptG_methods = {"B_KeyboardInterrupt",UNASSIGNED,($SuperG_class)&B_BaseExceptionG_methods,B_KeyboardInterruptD___init__,
                                                              B_KeyboardInterruptD___serialize__,B_KeyboardInterruptD___deserialize__,
                                                              B_KeyboardInterruptD___bool__,B_KeyboardInterruptD___str__,B_KeyboardInterruptD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_Exception B_ExceptionG_new(B_str error_message) {
  return $NEW(B_Exception, error_message);
}

void B_ExceptionD___init__(B_Exception self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_ExceptionD___bool__(B_Exception self) {
  return B_True;
}

B_str B_ExceptionD___str__(B_Exception self) {
  char *s;
  asprintf(&s,"Exception:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_ExceptionD___serialize__(B_Exception self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_Exception B_ExceptionD___deserialize__(B_Exception self, $Serial$state state) {
  B_Exception res = $DNEW(B_Exception,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_ExceptionG_class B_ExceptionG_methods = {"B_Exception",UNASSIGNED,($SuperG_class)&B_BaseExceptionG_methods,B_ExceptionD___init__,
                                              B_ExceptionD___serialize__,B_ExceptionD___deserialize__,B_ExceptionD___bool__,
                                              B_ExceptionD___str__, B_ExceptionD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_AssertionError B_AssertionErrorG_new(B_str error_message) {
  return $NEW(B_AssertionError, error_message);
}

void B_AssertionErrorD___init__(B_AssertionError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_AssertionErrorD___bool__(B_AssertionError self) {
  return B_True;
}

B_str B_AssertionErrorD___str__(B_AssertionError self) {
  char *s;
  asprintf(&s,"AssertionError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_AssertionErrorD___serialize__(B_AssertionError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_AssertionError B_AssertionErrorD___deserialize__(B_AssertionError self, $Serial$state state) {
  B_AssertionError res = $DNEW(B_AssertionError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_AssertionErrorG_class B_AssertionErrorG_methods = {"B_AssertionError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_AssertionErrorD___init__,
                                                        B_AssertionErrorD___serialize__,B_AssertionErrorD___deserialize__,B_AssertionErrorD___bool__,
                                                        B_AssertionErrorD___str__,B_AssertionErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_LookupError B_LookupErrorG_new(B_str error_message) {
  return $NEW(B_LookupError, error_message);
}

void B_LookupErrorD___init__(B_LookupError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_LookupErrorD___bool__(B_LookupError self) {
  return B_True;
}

B_str B_LookupErrorD___str__(B_LookupError self) {
  char *s;
  asprintf(&s,"LookupError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_LookupErrorD___serialize__(B_LookupError self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_LookupError B_LookupErrorD___deserialize__(B_LookupError self,$Serial$state state) {
  B_LookupError res = $DNEW(B_LookupError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_LookupErrorG_class B_LookupErrorG_methods = {"B_LookupError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_LookupErrorD___init__,
                                                  B_LookupErrorD___serialize__,B_LookupErrorD___deserialize__,B_LookupErrorD___bool__,
                                                  B_LookupErrorD___str__,B_LookupErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_IndexError B_IndexErrorG_new(B_str error_message) {
  return $NEW(B_IndexError, error_message);
}

void B_IndexErrorD___init__(B_IndexError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_IndexErrorD___bool__(B_IndexError self) {
  return B_True;
}

B_str B_IndexErrorD___str__(B_IndexError self) {
  char *s;
  asprintf(&s,"IndexError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_IndexErrorD___serialize__(B_IndexError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_IndexError B_IndexErrorD___deserialize__(B_IndexError self, $Serial$state state) {
  B_IndexError res = $DNEW(B_IndexError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_IndexErrorG_class B_IndexErrorG_methods = {"B_IndexError",UNASSIGNED,($SuperG_class)&B_LookupErrorG_methods,B_IndexErrorD___init__,
                                                B_IndexErrorD___serialize__,B_IndexErrorD___deserialize__,B_IndexErrorD___bool__,
                                                B_IndexErrorD___str__,B_IndexErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_KeyError B_KeyErrorG_new(B_str error_message) {
  return $NEW(B_KeyError, error_message);
}

void B_KeyErrorD___init__(B_KeyError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_KeyErrorD___bool__(B_KeyError self) {
  return B_True;
}

B_str B_KeyErrorD___str__(B_KeyError self) {
  char *s;
  asprintf(&s,"KeyError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_KeyErrorD___serialize__(B_KeyError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_KeyError B_KeyErrorD___deserialize__(B_KeyError self, $Serial$state state) {
  B_KeyError res = $DNEW(B_KeyError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_KeyErrorG_class B_KeyErrorG_methods = {"B_KeyError",UNASSIGNED,($SuperG_class)&B_LookupErrorG_methods,B_KeyErrorD___init__,
                                            B_KeyErrorD___serialize__,B_KeyErrorD___deserialize__,B_KeyErrorD___bool__,
                                            B_KeyErrorD___str__,B_KeyErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_MemoryError B_MemoryErrorG_new(B_str error_message) {
  return $NEW(B_MemoryError, error_message);
}

void B_MemoryErrorD___init__(B_MemoryError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_MemoryErrorD___bool__(B_MemoryError self) {
  return B_True;
}

B_str B_MemoryErrorD___str__(B_MemoryError self) {
  char *s;
  asprintf(&s,"MemoryError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_MemoryErrorD___serialize__(B_MemoryError self, $Serial$state state) {
    $add_header(MEMORYERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

B_MemoryError B_MemoryErrorD___deserialize__(B_MemoryError self, $Serial$state state) {
  B_MemoryError res = $DNEW(B_MemoryError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_MemoryErrorG_class B_MemoryErrorG_methods = {"B_MemoryError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_MemoryErrorD___init__,
                                                  B_MemoryErrorD___serialize__,B_MemoryErrorD___deserialize__,B_MemoryErrorD___bool__,
                                                  B_MemoryErrorD___str__,B_MemoryErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_OSError B_OSErrorG_new(B_str error_message) {
  return $NEW(B_OSError, error_message);
}

void B_OSErrorD___init__(B_OSError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_OSErrorD___bool__(B_OSError self) {
  return B_True;
}

B_str B_OSErrorD___str__(B_OSError self) {
  char *s;
  asprintf(&s,"OSError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_OSErrorD___serialize__(B_OSError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_OSError B_OSErrorD___deserialize__(B_OSError self, $Serial$state state) {
  B_OSError res = $DNEW(B_OSError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_OSErrorG_class B_OSErrorG_methods = {"B_OSError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_OSErrorD___init__,
                                          B_OSErrorD___serialize__,B_OSErrorD___deserialize__,B_OSErrorD___bool__,
                                          B_OSErrorD___str__,B_OSErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////
B_RuntimeError B_RuntimeErrorG_new(B_str error_message) {
    return $NEW(B_RuntimeError, error_message);
}

void B_RuntimeErrorD___init__(B_RuntimeError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_RuntimeErrorD___bool__(B_RuntimeError self) {
  return B_True;
}

B_str B_RuntimeErrorD___str__(B_RuntimeError self) {
  char *s;
  asprintf(&s,"RuntimeError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_RuntimeErrorD___serialize__(B_RuntimeError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

B_RuntimeError B_RuntimeErrorD___deserialize__(B_RuntimeError self, $Serial$state state) {
  B_RuntimeError res = $DNEW(B_RuntimeError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_RuntimeErrorG_class B_RuntimeErrorG_methods = {"B_RuntimeError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_RuntimeErrorD___init__,
                                                    B_RuntimeErrorD___serialize__,B_RuntimeErrorD___deserialize__,B_RuntimeErrorD___bool__,
                                                    B_RuntimeErrorD___str__,B_RuntimeErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////
void B_NotImplementedErrorD___init__(B_NotImplementedError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_NotImplementedErrorD___bool__(B_NotImplementedError self) {
  return B_True;
}

B_str B_NotImplementedErrorD___str__(B_NotImplementedError self) {
  char *s;
  asprintf(&s,"NotImplementedError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_NotImplementedErrorD___serialize__(B_NotImplementedError self, $Serial$state state) {
    $add_header(NOTIMPLEMENTEDERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

B_NotImplementedError B_NotImplementedErrorD___deserialize__(B_NotImplementedError self, $Serial$state state) {
  B_NotImplementedError res = $DNEW(B_NotImplementedError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_NotImplementedErrorG_class B_NotImplementedErrorG_methods = {"B_NotImplementedError",UNASSIGNED,($SuperG_class)&B_RuntimeErrorG_methods,B_NotImplementedErrorD___init__,
                                                                  B_NotImplementedErrorD___serialize__,B_NotImplementedErrorD___deserialize__,B_NotImplementedErrorD___bool__,
                                                                  B_NotImplementedErrorD___str__,B_NotImplementedErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

B_ValueError B_ValueErrorG_new(B_str error_message) {
  return $NEW(B_ValueError, error_message);
}

void B_ValueErrorD___init__(B_ValueError self, B_str error_message) {
  self->error_message = error_message;
};

B_bool B_ValueErrorD___bool__(B_ValueError self) {
  return B_True;
}

B_str B_ValueErrorD___str__(B_ValueError self) {
  char *s;
  asprintf(&s,"ValueError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void B_ValueErrorD___serialize__(B_ValueError self,$Serial$state state) {
    $add_header(VALUEERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

B_ValueError B_ValueErrorD___deserialize__(B_ValueError self,$Serial$state state) {
  B_ValueError res = $DNEW(B_ValueError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct B_ValueErrorG_class B_ValueErrorG_methods = {"B_ValueError",UNASSIGNED,($SuperG_class)&B_ExceptionG_methods,B_ValueErrorD___init__,
                                                B_ValueErrorD___serialize__,B_ValueErrorD___deserialize__,B_ValueErrorD___bool__,
                                                B_ValueErrorD___str__,B_ValueErrorD___str__};
//////////////////////////////////////////////////////////////////////////////////////////////

void $RAISE(B_BaseException e) {
  fprintf(stderr,"%s\n",(char*)fromB_str(e->error_message));
  exit(1);
}
