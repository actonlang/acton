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

$Serializable $Serializable$new() {
  return $NEW($Serializable);
}

void $Serializable$__init__ ($Serializable self) {
  return;
}

$value $value$new() {
  return $NEW($value);
}

void $value$__init__ ($value self) {
  return;
}

$object $object$new() {
  return $NEW($object);
}

void $object$__init__ ($object self) {
  return;
}



struct $Initializable$class $Initializable$methods = {"$Initializable",UNASSIGNED,NULL,NULL};

struct $Serializable$class $Serializable$methods = {"$Serializable",UNASSIGNED,($Super$class)&$Initializable$methods, $Serializable$__init__,NULL,NULL};

struct $value$class $value$methods = {"$value",UNASSIGNED,($Super$class)&$Serializable$methods,$value$__init__,NULL,NULL,NULL,NULL,NULL};

struct $object$class $object$methods = {"$value",UNASSIGNED,($Super$class)&$value$methods,$object$__init__,NULL,NULL,NULL,NULL,NULL};
