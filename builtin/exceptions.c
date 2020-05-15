void $BaseException$__init__($BaseException self, $str error_message) {
  self->error_message = error_message;
};

void $BaseException$__serialize__($BaseException self, $Serial$state state) {
    $enqueue(state,$new_row(BASEEXCEPTION_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$BaseException $BaseException$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($BaseException,($str)$step_deserialize(state));
};

struct $BaseException$class $BaseException$methods = {"",UNASSIGNED,NULL,$BaseException$__init__,$BaseException$__serialize__,$BaseException$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $SystemExit$__init__($SystemExit self, $str error_message) {
  self->error_message = error_message;
};

void $SystemExit$__serialize__($SystemExit self, $Serial$state state) {
    $enqueue(state,$new_row(SYSTEMEXIT_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$SystemExit $SystemExit$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($SystemExit,($str)$step_deserialize(state));
};

struct $SystemExit$class $SystemExit$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$SystemExit$__init__,$SystemExit$__serialize__,$SystemExit$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyboardInterrupt$__init__($KeyboardInterrupt self, $str error_message) {
  self->error_message = error_message;
};

void $KeyboardInterrupt$__serialize__($KeyboardInterrupt self,$Serial$state state) {
    $enqueue(state,$new_row(KEYBOARDINTERRUPT_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$KeyboardInterrupt $KeyboardInterrupt$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($KeyboardInterrupt,($str)$step_deserialize(state));
};

struct $KeyboardInterrupt$class $KeyboardInterrupt$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$KeyboardInterrupt$__init__,$KeyboardInterrupt$__serialize__,$KeyboardInterrupt$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $Exception$__init__($Exception self, $str error_message) {
  self->error_message = error_message;
};

void $Exception$__serialize__($Exception self,$Serial$state state) {
    $enqueue(state,$new_row(EXCEPTION_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$Exception $Exception$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($Exception,($str)$step_deserialize(state));
};

struct $Exception$class $Exception$methods = {"",UNASSIGNED,($Super$class)&$BaseException$methods,$Exception$__init__,$Exception$__serialize__,$Exception$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $AssertionError$__init__($AssertionError self, $str error_message) {
  self->error_message = error_message;
};

void $AssertionError$__serialize__($AssertionError self, $Serial$state state) {
    $enqueue(state,$new_row(ASSERTIONERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$AssertionError $AssertionError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  return $NEW($AssertionError,($str)$step_deserialize(state));
  state->row++;
};

struct $AssertionError$class $AssertionError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$AssertionError$__init__,$AssertionError$__serialize__,$AssertionError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $LookupError$__init__($LookupError self, $str error_message) {
  self->error_message = error_message;
};

void $LookupError$__serialize__($LookupError self,$Serial$state state) {
    $enqueue(state,$new_row(LOOKUPERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$LookupError $LookupError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($LookupError,($str)$step_deserialize(state));
};

struct $LookupError$class $LookupError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$LookupError$__init__,$LookupError$__serialize__,$LookupError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $IndexError$__init__($IndexError self, $str error_message) {
  self->error_message = error_message;
};

void $IndexError$__serialize__($IndexError self, $Serial$state state) {
    $enqueue(state,$new_row(INDEXERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$IndexError $IndexError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($IndexError,($str)$step_deserialize(state));
};

struct $IndexError$class $IndexError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$IndexError$__init__,$IndexError$__serialize__,$IndexError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyError$__init__($KeyError self, $str error_message) {
  self->error_message = error_message;
};

void $KeyError$__serialize__($KeyError self, $Serial$state state) {
    $enqueue(state,$new_row(KEYERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$KeyError $KeyError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($KeyError,($str)$step_deserialize(state));
};

struct $KeyError$class $KeyError$methods = {"",UNASSIGNED,($Super$class)&$LookupError$methods,$KeyError$__init__,$KeyError$__serialize__,$KeyError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $MemoryError$__init__($MemoryError self, $str error_message) {
  self->error_message = error_message;
};

void $MemoryError$__serialize__($MemoryError self, $Serial$state state) {
    $enqueue(state,$new_row(MEMORYERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$MemoryError $MemoryError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($MemoryError,($str)$step_deserialize(state));
};

struct $MemoryError$class $MemoryError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$MemoryError$__init__,$MemoryError$__serialize__,$MemoryError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $OSError$__init__($OSError self, $str error_message) {
  self->error_message = error_message;
};

void $OSError$__serialize__($OSError self, $Serial$state state) {
    $enqueue(state,$new_row(OSERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$OSError $OSError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($OSError,($str)$step_deserialize(state));
};

struct $OSError$class $OSError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$OSError$__init__,$OSError$__serialize__,$OSError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $RuntimeError$__init__($RuntimeError self, $str error_message) {
  self->error_message = error_message;
};

void $RuntimeError$__serialize__($RuntimeError self, $Serial$state state) {
    $enqueue(state,$new_row(RUNTIMEERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$RuntimeError $RuntimeError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($RuntimeError,($str)$step_deserialize(state));
};

struct $RuntimeError$class $RuntimeError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$RuntimeError$__init__,$RuntimeError$__serialize__,$RuntimeError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $NotImplementedError$__init__($NotImplementedError self, $str error_message) {
  self->error_message = error_message;
};

void $NotImplementedError$__serialize__($NotImplementedError self, $Serial$state state) {
    $enqueue(state,$new_row(NOTIMPLEMENTEDERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$NotImplementedError $NotImplementedError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($NotImplementedError,($str)$step_deserialize(state));
};

struct $NotImplementedError$class $NotImplementedError$methods = {"",UNASSIGNED,($Super$class)&$RuntimeError$methods,$NotImplementedError$__init__,$NotImplementedError$__serialize__,$NotImplementedError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $ValueError$__init__($ValueError self, $str error_message) {
  self->error_message = error_message;
};

void $ValueError$__serialize__($ValueError self,$Serial$state state) {
    $enqueue(state,$new_row(VALUEERROR_ID,&state->row_no,0,NULL));
    $step_serialize(self->error_message,state);
};

$ValueError $ValueError$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row++;
  return $NEW($ValueError,($str)$step_deserialize(state));
};

struct $ValueError$class $ValueError$methods = {"",UNASSIGNED,($Super$class)&$Exception$methods,$ValueError$__init__,$ValueError$__serialize__,$ValueError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
