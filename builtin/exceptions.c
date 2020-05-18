void $BaseException$__init__($BaseException self, $str error_message) {
  self->error_message = error_message;
};

void $BaseException$__serialize__($BaseException self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$BaseException $BaseException$__deserialize__($Serial$state state) {
  $BaseException res = $DNEW($BaseException,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $BaseException$class $BaseException$methods = {"",UNASSIGNED,NULL,$BaseException$__init__,$BaseException$__serialize__,$BaseException$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $SystemExit$__init__($SystemExit self, $str error_message) {
  self->error_message = error_message;
};

void $SystemExit$__serialize__($SystemExit self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$SystemExit $SystemExit$__deserialize__($Serial$state state) {
  $SystemExit res = $DNEW($SystemExit,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $SystemExit$class $SystemExit$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$SystemExit$__init__,$SystemExit$__serialize__,$SystemExit$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyboardInterrupt$__init__($KeyboardInterrupt self, $str error_message) {
  self->error_message = error_message;
};

void $KeyboardInterrupt$__serialize__($KeyboardInterrupt self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyboardInterrupt $KeyboardInterrupt$__deserialize__($Serial$state state) {
  $KeyboardInterrupt res = $DNEW($KeyboardInterrupt,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyboardInterrupt$class $KeyboardInterrupt$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$KeyboardInterrupt$__init__,$KeyboardInterrupt$__serialize__,$KeyboardInterrupt$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $Exception$__init__($Exception self, $str error_message) {
  self->error_message = error_message;
};

void $Exception$__serialize__($Exception self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$Exception $Exception$__deserialize__($Serial$state state) {
  $Exception res = $DNEW($Exception,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $Exception$class $Exception$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$Exception$__init__,$Exception$__serialize__,$Exception$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $AssertionError$__init__($AssertionError self, $str error_message) {
  self->error_message = error_message;
};

void $AssertionError$__serialize__($AssertionError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$AssertionError $AssertionError$__deserialize__($Serial$state state) {
  $AssertionError res = $DNEW($AssertionError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $AssertionError$class $AssertionError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$AssertionError$__init__,$AssertionError$__serialize__,$AssertionError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $LookupError$__init__($LookupError self, $str error_message) {
  self->error_message = error_message;
};

void $LookupError$__serialize__($LookupError self,$Serial$state state) {
    $step_serialize(self->error_message,state);
};

$LookupError $LookupError$__deserialize__($Serial$state state) {
  $LookupError res = $DNEW($LookupError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $LookupError$class $LookupError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$LookupError$__init__,$LookupError$__serialize__,$LookupError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $IndexError$__init__($IndexError self, $str error_message) {
  self->error_message = error_message;
};

void $IndexError$__serialize__($IndexError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$IndexError $IndexError$__deserialize__($Serial$state state) {
  $IndexError res = $DNEW($IndexError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $IndexError$class $IndexError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$IndexError$__init__,$IndexError$__serialize__,$IndexError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyError$__init__($KeyError self, $str error_message) {
  self->error_message = error_message;
};

void $KeyError$__serialize__($KeyError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$KeyError $KeyError$__deserialize__($Serial$state state) {
  $KeyError res = $DNEW($KeyError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $KeyError$class $KeyError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$KeyError$__init__,$KeyError$__serialize__,$KeyError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $MemoryError$__init__($MemoryError self, $str error_message) {
  self->error_message = error_message;
};

void $MemoryError$__serialize__($MemoryError self, $Serial$state state) {
    $add_header(MEMORYERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$MemoryError $MemoryError$__deserialize__($Serial$state state) {
  $MemoryError res = $DNEW($MemoryError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $MemoryError$class $MemoryError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$MemoryError$__init__,$MemoryError$__serialize__,$MemoryError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $OSError$__init__($OSError self, $str error_message) {
  self->error_message = error_message;
};

void $OSError$__serialize__($OSError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$OSError $OSError$__deserialize__($Serial$state state) {
  $OSError res = $DNEW($OSError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $OSError$class $OSError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$OSError$__init__,$OSError$__serialize__,$OSError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $RuntimeError$__init__($RuntimeError self, $str error_message) {
  self->error_message = error_message;
};

void $RuntimeError$__serialize__($RuntimeError self, $Serial$state state) {
    $step_serialize(self->error_message,state);
};

$RuntimeError $RuntimeError$__deserialize__($Serial$state state) {
  $RuntimeError res = $DNEW($RuntimeError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $RuntimeError$class $RuntimeError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$RuntimeError$__init__,$RuntimeError$__serialize__,$RuntimeError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $NotImplementedError$__init__($NotImplementedError self, $str error_message) {
  self->error_message = error_message;
};

void $NotImplementedError$__serialize__($NotImplementedError self, $Serial$state state) {
    $add_header(NOTIMPLEMENTEDERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$NotImplementedError $NotImplementedError$__deserialize__($Serial$state state) {
  $NotImplementedError res = $DNEW($NotImplementedError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $NotImplementedError$class $NotImplementedError$methods = {"",UNASSIGNED,($Super$class)&$RuntimeError$methods,$NotImplementedError$__init__,$NotImplementedError$__serialize__,$NotImplementedError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $ValueError$__init__($ValueError self, $str error_message) {
  self->error_message = error_message;
};

void $ValueError$__serialize__($ValueError self,$Serial$state state) {
    $add_header(VALUEERROR_ID,0,state);
    $step_serialize(self->error_message,state);
};

$ValueError $ValueError$__deserialize__($Serial$state state) {
  $ValueError res = $DNEW($ValueError,state);
  res->error_message = $step_deserialize(state);
  return res;
};

struct $ValueError$class $ValueError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$ValueError$__init__,$ValueError$__serialize__,$ValueError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
