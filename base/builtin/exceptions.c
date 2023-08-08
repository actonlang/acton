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

B_str B_BaseExceptionD___str__(B_BaseException self) {
  char *s;
  asprintf(&s,"BaseException:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_SystemExitD___str__(B_SystemExit self) {
  char *s;
  asprintf(&s,"SystemExit:  %s>",fromB_str(self->error_message));
  return to$str(s);
}
 
B_str B_KeyboardInterruptD___str__(B_KeyboardInterrupt self) {
  char *s;
  asprintf(&s,"KeyboardInterrupt:  %s>",fromB_str(self->error_message));
  return to$str(s);
}
 
B_str B_ExceptionD___str__(B_Exception self) {
  char *s;
  asprintf(&s,"Exception:  %s>",fromB_str(self->error_message));
  return to$str(s);
}
 
B_str B_AssertionErrorD___str__(B_AssertionError self) {
  char *s;
  asprintf(&s,"AssertionError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_LookupErrorD___str__(B_LookupError self) {
  char *s;
  asprintf(&s,"LookupError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_IndexErrorD___str__(B_IndexError self) {
  char *s;
  asprintf(&s,"IndexError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_KeyErrorD___str__(B_KeyError self) {
  char *s;
  asprintf(&s,"KeyError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_MemoryErrorD___str__(B_MemoryError self) {
  char *s;
  asprintf(&s,"MemoryError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_OSErrorD___str__(B_OSError self) {
  char *s;
  asprintf(&s,"OSError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_RuntimeErrorD___str__(B_RuntimeError self) {
  char *s;
  asprintf(&s,"RuntimeError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_NotImplementedErrorD___str__(B_NotImplementedError self) {
  char *s;
  asprintf(&s,"NotImplementedError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

B_str B_ValueErrorD___str__(B_ValueError self) {
  char *s;
  asprintf(&s,"ValueError:  %s>",fromB_str(self->error_message));
  return to$str(s);
}

void $RAISE(B_BaseException e) {
  fprintf(stderr, "%s: %s\n", e->$class->$GCINFO, e->error_message ? fromB_str(e->error_message) : "");
  exit(1);
}

