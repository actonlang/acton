void $BaseException$__init__($BaseException self, $str error_message) {
  self->error_message = error_message;
};

void $BaseException$__serialize__($BaseException self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(BASEEXCEPTION_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$BaseException $BaseException$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($BaseException,($str)$step_deserialize(wit,row,done));
};

struct $BaseException$class $BaseException$methods = {"",NULL,$BaseException$__init__,$BaseException$__serialize__,$BaseException$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $SystemExit$__init__($SystemExit self, $str error_message) {
  self->error_message = error_message;
};

void $SystemExit$__serialize__($SystemExit self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(SYSTEMEXIT_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$SystemExit $SystemExit$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($SystemExit,($str)$step_deserialize(wit,row,done));
};

struct $SystemExit$class $SystemExit$methods = {"",($Super$class)&$BaseException$methods,$SystemExit$__init__,$SystemExit$__serialize__,$SystemExit$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyboardInterrupt$__init__($KeyboardInterrupt self, $str error_message) {
  self->error_message = error_message;
};

void $KeyboardInterrupt$__serialize__($KeyboardInterrupt self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(KEYBOARDINTERRUPT_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$KeyboardInterrupt $KeyboardInterrupt$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($KeyboardInterrupt,($str)$step_deserialize(wit,row,done));
};

struct $KeyboardInterrupt$class $KeyboardInterrupt$methods = {"",($Super$class)&$BaseException$methods,$KeyboardInterrupt$__init__,$KeyboardInterrupt$__serialize__,$KeyboardInterrupt$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $Exception$__init__($Exception self, $str error_message) {
  self->error_message = error_message;
};

void $Exception$__serialize__($Exception self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(EXCEPTION_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$Exception $Exception$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($Exception,($str)$step_deserialize(wit,row,done));
};

struct $Exception$class $Exception$methods = {"",($Super$class)&$BaseException$methods,$Exception$__init__,$Exception$__serialize__,$Exception$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $AssertionError$__init__($AssertionError self, $str error_message) {
  self->error_message = error_message;
};

void $AssertionError$__serialize__($AssertionError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(ASSERTIONERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$AssertionError $AssertionError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($AssertionError,($str)$step_deserialize(wit,row,done));
};

struct $AssertionError$class $AssertionError$methods = {"",($Super$class)&$Exception$methods,$AssertionError$__init__,$AssertionError$__serialize__,$AssertionError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $LookupError$__init__($LookupError self, $str error_message) {
  self->error_message = error_message;
};

void $LookupError$__serialize__($LookupError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(LOOKUPERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$LookupError $LookupError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($LookupError,($str)$step_deserialize(wit,row,done));
};

struct $LookupError$class $LookupError$methods = {"",($Super$class)&$Exception$methods,$LookupError$__init__,$LookupError$__serialize__,$LookupError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $IndexError$__init__($IndexError self, $str error_message) {
  self->error_message = error_message;
};

void $IndexError$__serialize__($IndexError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(INDEXERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$IndexError $IndexError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($IndexError,($str)$step_deserialize(wit,row,done));
};

struct $IndexError$class $IndexError$methods = {"",($Super$class)&$LookupError$methods,$IndexError$__init__,$IndexError$__serialize__,$IndexError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $KeyError$__init__($KeyError self, $str error_message) {
  self->error_message = error_message;
};

void $KeyError$__serialize__($KeyError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(KEYERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$KeyError $KeyError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($KeyError,($str)$step_deserialize(wit,row,done));
};

struct $KeyError$class $KeyError$methods = {"",($Super$class)&$LookupError$methods,$KeyError$__init__,$KeyError$__serialize__,$KeyError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $MemoryError$__init__($MemoryError self, $str error_message) {
  self->error_message = error_message;
};

void $MemoryError$__serialize__($MemoryError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(MEMORYERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$MemoryError $MemoryError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($MemoryError,($str)$step_deserialize(wit,row,done));
};

struct $MemoryError$class $MemoryError$methods = {"",($Super$class)&$Exception$methods,$MemoryError$__init__,$MemoryError$__serialize__,$MemoryError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $OSError$__init__($OSError self, $str error_message) {
  self->error_message = error_message;
};

void $OSError$__serialize__($OSError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(OSERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$OSError $OSError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($OSError,($str)$step_deserialize(wit,row,done));
};

struct $OSError$class $OSError$methods = {"",($Super$class)&$Exception$methods,$OSError$__init__,$OSError$__serialize__,$OSError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $RuntimeError$__init__($RuntimeError self, $str error_message) {
  self->error_message = error_message;
};

void $RuntimeError$__serialize__($RuntimeError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(RUNTIMEERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$RuntimeError $RuntimeError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($RuntimeError,($str)$step_deserialize(wit,row,done));
};

struct $RuntimeError$class $RuntimeError$methods = {"",($Super$class)&$Exception$methods,$RuntimeError$__init__,$RuntimeError$__serialize__,$RuntimeError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $NotImplementedError$__init__($NotImplementedError self, $str error_message) {
  self->error_message = error_message;
};

void $NotImplementedError$__serialize__($NotImplementedError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(NOTIMPLEMENTEDERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$NotImplementedError $NotImplementedError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($NotImplementedError,($str)$step_deserialize(wit,row,done));
};

struct $NotImplementedError$class $NotImplementedError$methods = {"",($Super$class)&$RuntimeError$methods,$NotImplementedError$__init__,$NotImplementedError$__serialize__,$NotImplementedError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
void $ValueError$__init__($ValueError self, $str error_message) {
  self->error_message = error_message;
};

void $ValueError$__serialize__($ValueError self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
    $enqueue(accum,$new_row(VALUEERROR_ID,start_no,0,NULL));
    $step_serialize(($Serializable)self->error_message,wit,start_no,done,accum);
};

$ValueError $ValueError$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  return $NEW($ValueError,($str)$step_deserialize(wit,row,done));
};

struct $ValueError$class $ValueError$methods = {"",($Super$class)&$Exception$methods,$ValueError$__init__,$ValueError$__serialize__,$ValueError$__deserialize__};
//////////////////////////////////////////////////////////////////////////////////////////////
