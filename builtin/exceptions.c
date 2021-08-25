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

$BaseException $BaseException$new($str error_message) {
  return $NEW($BaseException, error_message);
}

void $BaseException$__init__($BaseException self, $str error_message) {
  self->error_message = error_message;
};

$bool $BaseException$__bool__($BaseException self) {
  return $True;
}

$str $BaseException$__str__($BaseException self) {
  char *s;
  asprintf(&s,"BaseException:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $BaseException$__serialize__($BaseException self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$BaseException $BaseException$__deserialize__($BaseException self, $Serial$state state) {
  $BaseException res = $DNEW($BaseException,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $BaseException$class $BaseException$methods = {"$BaseException",UNASSIGNED,NULL,$BaseException$__init__,
                                                      $BaseException$__serialize__,$BaseException$__deserialize__,$BaseException$__bool__,$BaseException$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$SystemExit $SystemExit$new($str error_message) {
  return $NEW($SystemExit, error_message);
}


void $SystemExit$__init__($SystemExit self, $str error_message) {
  self->error_message = error_message;
};

$bool $SystemExit$__bool__($SystemExit self) {
  return $True;
}

$str $SystemExit$__str__($SystemExit self) {
  char *s;
  asprintf(&s,"SystemExit:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $SystemExit$__serialize__($SystemExit self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$SystemExit $SystemExit$__deserialize__($SystemExit self, $Serial$state state) {
  $SystemExit res = $DNEW($SystemExit,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $SystemExit$class $SystemExit$methods = {"$SystemExit",UNASSIGNED,($Super$class)&$BaseException$methods,$SystemExit$__init__,
                                               $SystemExit$__serialize__,$SystemExit$__deserialize__,$SystemExit$__bool__,$SystemExit$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$KeyboardInterrupt $KeyboardInterrupt$new($str error_message) {
  return $NEW($KeyboardInterrupt, error_message);
}

void $KeyboardInterrupt$__init__($KeyboardInterrupt self, $str error_message) {
  self->error_message = error_message;
};

$bool $KeyboardInterrupt$__bool__($KeyboardInterrupt self) {
  return $True;
}

$str $KeyboardInterrupt$__str__($KeyboardInterrupt self) {
  char *s;
  asprintf(&s,"KeyboardInterrupt:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $KeyboardInterrupt$__serialize__($KeyboardInterrupt self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyboardInterrupt $KeyboardInterrupt$__deserialize__($KeyboardInterrupt self, $Serial$state state) {
  $KeyboardInterrupt res = $DNEW($KeyboardInterrupt,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyboardInterrupt$class $KeyboardInterrupt$methods = {"$KeyboardInterrupt",UNASSIGNED,($Super$class)&$BaseException$methods,$KeyboardInterrupt$__init__,
                                                              $KeyboardInterrupt$__serialize__,$KeyboardInterrupt$__deserialize__,$KeyboardInterrupt$__bool__,$KeyboardInterrupt$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$Exception $Exception$new($str error_message) {
  return $NEW($Exception, error_message);
}

void $Exception$__init__($Exception self, $str error_message) {
  self->error_message = error_message;
};

$bool $Exception$__bool__($Exception self) {
  return $True;
}

$str $Exception$__str__($Exception self) {
  char *s;
  asprintf(&s,"Exception:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $Exception$__serialize__($Exception self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$Exception $Exception$__deserialize__($Exception self, $Serial$state state) {
  $Exception res = $DNEW($Exception,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $Exception$class $Exception$methods = {"$Exception",UNASSIGNED,($Super$class)&$BaseException$methods,$Exception$__init__,
                                              $Exception$__serialize__,$Exception$__deserialize__,$Exception$__bool__, $Exception$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$AssertionError $AssertionError$new($str error_message) {
  return $NEW($AssertionError, error_message);
}

void $AssertionError$__init__($AssertionError self, $str error_message) {
  self->error_message = error_message;
};

$bool $AssertionError$__bool__($AssertionError self) {
  return $True;
}

$str $AssertionError$__str__($AssertionError self) {
  char *s;
  asprintf(&s,"AssertionError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $AssertionError$__serialize__($AssertionError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$AssertionError $AssertionError$__deserialize__($AssertionError self, $Serial$state state) {
  $AssertionError res = $DNEW($AssertionError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $AssertionError$class $AssertionError$methods = {"$AssertionError",UNASSIGNED,($Super$class)&$Exception$methods,$AssertionError$__init__,
                                                        $AssertionError$__serialize__,$AssertionError$__deserialize__,$AssertionError$__bool__,$AssertionError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$LookupError $LookupError$new($str error_message) {
  return $NEW($LookupError, error_message);
}

void $LookupError$__init__($LookupError self, $str error_message) {
  self->error_message = error_message;
};

$bool $LookupError$__bool__($LookupError self) {
  return $True;
}

$str $LookupError$__str__($LookupError self) {
  char *s;
  asprintf(&s,"LookupError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $LookupError$__serialize__($LookupError self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$LookupError $LookupError$__deserialize__($LookupError self,$Serial$state state) {
  $LookupError res = $DNEW($LookupError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $LookupError$class $LookupError$methods = {"$LookupError",UNASSIGNED,($Super$class)&$Exception$methods,$LookupError$__init__,
                                                  $LookupError$__serialize__,$LookupError$__deserialize__,$LookupError$__bool__,$LookupError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$IndexError $IndexError$new($str error_message) {
  return $NEW($IndexError, error_message);
}

void $IndexError$__init__($IndexError self, $str error_message) {
  self->error_message = error_message;
};

$bool $IndexError$__bool__($IndexError self) {
  return $True;
}

$str $IndexError$__str__($IndexError self) {
  char *s;
  asprintf(&s,"IndexError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $IndexError$__serialize__($IndexError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$IndexError $IndexError$__deserialize__($IndexError self, $Serial$state state) {
  $IndexError res = $DNEW($IndexError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $IndexError$class $IndexError$methods = {"$IndexError",UNASSIGNED,($Super$class)&$LookupError$methods,$IndexError$__init__,$IndexError$__serialize__,$IndexError$__deserialize__,$IndexError$__bool__,$IndexError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$KeyError $KeyError$new($str error_message) {
  return $NEW($KeyError, error_message);
}

void $KeyError$__init__($KeyError self, $str error_message) {
  self->error_message = error_message;
};

$bool $KeyError$__bool__($KeyError self) {
  return $True;
}

$str $KeyError$__str__($KeyError self) {
  char *s;
  asprintf(&s,"KeyError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $KeyError$__serialize__($KeyError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyError $KeyError$__deserialize__($KeyError self, $Serial$state state) {
  $KeyError res = $DNEW($KeyError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyError$class $KeyError$methods = {"$KeyError",UNASSIGNED,($Super$class)&$LookupError$methods,$KeyError$__init__,
                                            $KeyError$__serialize__,$KeyError$__deserialize__,$KeyError$__bool__,$KeyError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$MemoryError $MemoryError$new($str error_message) {
  return $NEW($MemoryError, error_message);
}

void $MemoryError$__init__($MemoryError self, $str error_message) {
  self->error_message = error_message;
};

$bool $MemoryError$__bool__($MemoryError self) {
  return $True;
}

$str $MemoryError$__str__($MemoryError self) {
  char *s;
  asprintf(&s,"MemoryError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $MemoryError$__serialize__($MemoryError self, $Serial$state state) {
    $add_header(MEMORYERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$MemoryError $MemoryError$__deserialize__($MemoryError self, $Serial$state state) {
  $MemoryError res = $DNEW($MemoryError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $MemoryError$class $MemoryError$methods = {"$MemoryError",UNASSIGNED,($Super$class)&$Exception$methods,$MemoryError$__init__,$MemoryError$__serialize__,$MemoryError$__deserialize__,$MemoryError$__bool__,$MemoryError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$OSError $OSError$new($str error_message) {
  return $NEW($OSError, error_message);
}

void $OSError$__init__($OSError self, $str error_message) {
  self->error_message = error_message;
};

$bool $OSError$__bool__($OSError self) {
  return $True;
}

$str $OSError$__str__($OSError self) {
  char *s;
  asprintf(&s,"OSError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $OSError$__serialize__($OSError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$OSError $OSError$__deserialize__($OSError self, $Serial$state state) {
  $OSError res = $DNEW($OSError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $OSError$class $OSError$methods = {"$OSError",UNASSIGNED,($Super$class)&$Exception$methods,$OSError$__init__,
                                          $OSError$__serialize__,$OSError$__deserialize__,$OSError$__bool__,$OSError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $RuntimeError$__init__($RuntimeError self, $str error_message) {
  self->error_message = error_message;
};

$bool $RuntimeError$__bool__($RuntimeError self) {
  return $True;
}

$str $RuntimeError$__str__($RuntimeError self) {
  char *s;
  asprintf(&s,"RuntimeError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $RuntimeError$__serialize__($RuntimeError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$RuntimeError $RuntimeError$__deserialize__($RuntimeError self, $Serial$state state) {
  $RuntimeError res = $DNEW($RuntimeError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $RuntimeError$class $RuntimeError$methods = {"$RuntimeError",UNASSIGNED,($Super$class)&$Exception$methods,$RuntimeError$__init__,$RuntimeError$__serialize__,$RuntimeError$__deserialize__,$RuntimeError$__bool__,$RuntimeError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $NotImplementedError$__init__($NotImplementedError self, $str error_message) {
  self->error_message = error_message;
};

$bool $NotImplementedError$__bool__($NotImplementedError self) {
  return $True;
}

$str $NotImplementedError$__str__($NotImplementedError self) {
  char *s;
  asprintf(&s,"NotImplementedError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $NotImplementedError$__serialize__($NotImplementedError self, $Serial$state state) {
    $add_header(NOTIMPLEMENTEDERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$NotImplementedError $NotImplementedError$__deserialize__($NotImplementedError self, $Serial$state state) {
  $NotImplementedError res = $DNEW($NotImplementedError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $NotImplementedError$class $NotImplementedError$methods = {"$NotImplementedError",UNASSIGNED,($Super$class)&$RuntimeError$methods,$NotImplementedError$__init__,$NotImplementedError$__serialize__,
                                                                 $NotImplementedError$__deserialize__,$NotImplementedError$__bool__,$NotImplementedError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

$ValueError $ValueError$new($str error_message) {
  return $NEW($ValueError, error_message);
}

void $ValueError$__init__($ValueError self, $str error_message) {
  self->error_message = error_message;
};

$bool $ValueError$__bool__($ValueError self) {
  return $True;
}

$str $ValueError$__str__($ValueError self) {
  char *s;
  asprintf(&s,"ValueError:  %s>",from$str(self->error_message));
  return to$str(s);
}

void $ValueError$__serialize__($ValueError self,$Serial$state state) {
    $add_header(VALUEERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$ValueError $ValueError$__deserialize__($ValueError self,$Serial$state state) {
  $ValueError res = $DNEW($ValueError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $ValueError$class $ValueError$methods = {"$ValueError",UNASSIGNED,($Super$class)&$Exception$methods,$ValueError$__init__,$ValueError$__serialize__,$ValueError$__deserialize__,$ValueError$__bool__,$ValueError$__str__};
//////////////////////////////////////////////////////////////////////////////////////////////

void $RAISE($BaseException e) {
  fprintf(stderr,"%s\n",(char*)from$str(e->error_message));
  exit(1);
}
