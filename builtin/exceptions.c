void $BaseException$__init__($BaseException self, $str error_message) {
  self->error_message = error_message;
};

$bool $BaseException$__bool__($BaseException self) {
  return $true;
}

$str $BaseException$__str__($BaseException self) {
  char *s;
  asprintf(&s,"BaseException:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $BaseException$__serialize__($BaseException self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$BaseException $BaseException$__deserialize__($Serial$state state) {
  $BaseException res = $DNEW($BaseException,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $BaseException$class $BaseException$methods = {"",UNASSIGNED,NULL,$BaseException$__init__,$BaseException$__bool__,$BaseException$__str__,
                                                      $BaseException$__serialize__,$BaseException$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $SystemExit$__init__($SystemExit self, $str error_message) {
  self->error_message = error_message;
};

$bool $SystemExit$__bool__($SystemExit self) {
  return $true;
}

$str $SystemExit$__str__($SystemExit self) {
  char *s;
  asprintf(&s,"SystemExit:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $SystemExit$__serialize__($SystemExit self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$SystemExit $SystemExit$__deserialize__($Serial$state state) {
  $SystemExit res = $DNEW($SystemExit,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $SystemExit$class $SystemExit$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$SystemExit$__init__,$SystemExit$__bool__,
                                                $SystemExit$__str__,$SystemExit$__serialize__,$SystemExit$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyboardInterrupt$__init__($KeyboardInterrupt self, $str error_message) {
  self->error_message = error_message;
};

$bool $KeyboardInterrupt$__bool__($KeyboardInterrupt self) {
  return $true;
}

$str $KeyboardInterrupt$__str__($KeyboardInterrupt self) {
  char *s;
  asprintf(&s,"KeyboardInterrupt:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $KeyboardInterrupt$__serialize__($KeyboardInterrupt self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyboardInterrupt $KeyboardInterrupt$__deserialize__($Serial$state state) {
  $KeyboardInterrupt res = $DNEW($KeyboardInterrupt,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyboardInterrupt$class $KeyboardInterrupt$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$KeyboardInterrupt$__init__,
                                                              $KeyboardInterrupt$__bool__,$KeyboardInterrupt$__str__,$KeyboardInterrupt$__serialize__,$KeyboardInterrupt$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $Exception$__init__($Exception self, $str error_message) {
  self->error_message = error_message;
};

$bool $Exception$__bool__($Exception self) {
  return $true;
}

$str $Exception$__str__($Exception self) {
  char *s;
  asprintf(&s,"Exception:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $Exception$__serialize__($Exception self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$Exception $Exception$__deserialize__($Serial$state state) {
  $Exception res = $DNEW($Exception,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $Exception$class $Exception$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$Exception$__init__,$Exception$__bool__,
                                              $Exception$__str__,$Exception$__serialize__,$Exception$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $AssertionError$__init__($AssertionError self, $str error_message) {
  self->error_message = error_message;
};

$bool $AssertionError$__bool__($AssertionError self) {
  return $true;
}

$str $AssertionError$__str__($AssertionError self) {
  char *s;
  asprintf(&s,"AssertionError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $AssertionError$__serialize__($AssertionError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$AssertionError $AssertionError$__deserialize__($Serial$state state) {
  $AssertionError res = $DNEW($AssertionError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $AssertionError$class $AssertionError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$AssertionError$__init__,$AssertionError$__bool__,
                                                        $AssertionError$__str__,$AssertionError$__serialize__,$AssertionError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $LookupError$__init__($LookupError self, $str error_message) {
  self->error_message = error_message;
};

$bool $LookupError$__bool__($LookupError self) {
  return $true;
}

$str $LookupError$__str__($LookupError self) {
  char *s;
  asprintf(&s,"LookupError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $LookupError$__serialize__($LookupError self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$LookupError $LookupError$__deserialize__($Serial$state state) {
  $LookupError res = $DNEW($LookupError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $LookupError$class $LookupError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$LookupError$__init__,$LookupError$__bool__,$LookupError$__str__,
                                                  $LookupError$__serialize__,$LookupError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $IndexError$__init__($IndexError self, $str error_message) {
  self->error_message = error_message;
};

$bool $IndexError$__bool__($IndexError self) {
  return $true;
}

$str $IndexError$__str__($IndexError self) {
  char *s;
  asprintf(&s,"IndexError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $IndexError$__serialize__($IndexError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$IndexError $IndexError$__deserialize__($Serial$state state) {
  $IndexError res = $DNEW($IndexError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $IndexError$class $IndexError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$IndexError$__init__,$IndexError$__bool__,$IndexError$__str__,$IndexError$__serialize__,$IndexError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyError$__init__($KeyError self, $str error_message) {
  self->error_message = error_message;
};

$bool $KeyError$__bool__($KeyError self) {
  return $true;
}

$str $KeyError$__str__($KeyError self) {
  char *s;
  asprintf(&s,"KeyError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $KeyError$__serialize__($KeyError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyError $KeyError$__deserialize__($Serial$state state) {
  $KeyError res = $DNEW($KeyError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyError$class $KeyError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$KeyError$__init__,$KeyError$__bool__,$KeyError$__str__,
                                            $KeyError$__serialize__,$KeyError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $MemoryError$__init__($MemoryError self, $str error_message) {
  self->error_message = error_message;
};

$bool $MemoryError$__bool__($MemoryError self) {
  return $true;
}

$str $MemoryError$__str__($MemoryError self) {
  char *s;
  asprintf(&s,"MemoryError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $MemoryError$__serialize__($MemoryError self, $Serial$state state) {
    $add_header(MEMORYERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$MemoryError $MemoryError$__deserialize__($Serial$state state) {
  $MemoryError res = $DNEW($MemoryError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $MemoryError$class $MemoryError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$MemoryError$__init__,$MemoryError$__bool__,$MemoryError$__str__,$MemoryError$__serialize__,$MemoryError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $OSError$__init__($OSError self, $str error_message) {
  self->error_message = error_message;
};

$bool $OSError$__bool__($OSError self) {
  return $true;
}

$str $OSError$__str__($OSError self) {
  char *s;
  asprintf(&s,"OSError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $OSError$__serialize__($OSError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$OSError $OSError$__deserialize__($Serial$state state) {
  $OSError res = $DNEW($OSError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $OSError$class $OSError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$OSError$__init__,$OSError$__bool__,$OSError$__str__,
                                          $OSError$__serialize__,$OSError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $RuntimeError$__init__($RuntimeError self, $str error_message) {
  self->error_message = error_message;
};

$bool $RuntimeError$__bool__($RuntimeError self) {
  return $true;
}

$str $RuntimeError$__str__($RuntimeError self) {
  char *s;
  asprintf(&s,"RuntimeError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $RuntimeError$__serialize__($RuntimeError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$RuntimeError $RuntimeError$__deserialize__($Serial$state state) {
  $RuntimeError res = $DNEW($RuntimeError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $RuntimeError$class $RuntimeError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$RuntimeError$__init__,$RuntimeError$__bool__,$RuntimeError$__str__,$RuntimeError$__serialize__,$RuntimeError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $NotImplementedError$__init__($NotImplementedError self, $str error_message) {
  self->error_message = error_message;
};

$bool $NotImplementedError$__bool__($NotImplementedError self) {
  return $true;
}

$str $NotImplementedError$__str__($NotImplementedError self) {
  char *s;
  asprintf(&s,"NotImplementedError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $NotImplementedError$__serialize__($NotImplementedError self, $Serial$state state) {
    $add_header(NOTIMPLEMENTEDERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$NotImplementedError $NotImplementedError$__deserialize__($Serial$state state) {
  $NotImplementedError res = $DNEW($NotImplementedError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $NotImplementedError$class $NotImplementedError$methods = {"",UNASSIGNED,($Super$class)&$RuntimeError$methods,$NotImplementedError$__init__,$NotImplementedError$__bool__,
                                                                  $NotImplementedError$__str__,$NotImplementedError$__serialize__,$NotImplementedError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $ValueError$__init__($ValueError self, $str error_message) {
  self->error_message = error_message;
};

$bool $ValueError$__bool__($ValueError self) {
  return $true;
}

$str $ValueError$__str__($ValueError self) {
  char *s;
  asprintf(&s,"ValueError:  %s>",to$UTF8(self->error_message));
  return from$UTF8(s);
}

void $ValueError$__serialize__($ValueError self,$Serial$state state) {
    $add_header(VALUEERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$ValueError $ValueError$__deserialize__($Serial$state state) {
  $ValueError res = $DNEW($ValueError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $ValueError$class $ValueError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$ValueError$__init__,$ValueError$__bool__,$ValueError$__str__,$ValueError$__serialize__,$ValueError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////

void RAISE($BaseException e) {
  fprintf(stderr,"%s\n",(char*)to$UTF8(e->error_message));
  exit(1);
}
