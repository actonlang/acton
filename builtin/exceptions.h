struct $BaseException$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($BaseException,$str);
  void (*__serialize__)($BaseException, $Serial$state);
  $BaseException (*__deserialize__)($BaseException, $Serial$state);
  $bool (*__bool__)($BaseException);
  $str (*__str__)($BaseException);
  $str (*__repr__)($BaseException);
};

typedef struct $BaseException *$BaseException;

struct $BaseException {
  struct $BaseException$class *$class;
  $str error_message;
};

extern struct $BaseException$class $BaseException$methods;
$BaseException $BaseException$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $SystemExit$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($SystemExit,$str);
  void (*__serialize__)($SystemExit,$Serial$state);
  $SystemExit (*__deserialize__)($SystemExit,$Serial$state);
  $bool (*__bool__)($SystemExit);
  $str (*__str__)($SystemExit);
  $str (*__repr__)($SystemExit);
};

typedef struct $SystemExit *$SystemExit;

struct $SystemExit {
  struct $SystemExit$class *$class;
  $str error_message;
};

extern struct $SystemExit$class $SystemExit$methods;
$SystemExit $SystemExit$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyboardInterrupt$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($KeyboardInterrupt,$str);
  void (*__serialize__)($KeyboardInterrupt,$Serial$state);
  $KeyboardInterrupt (*__deserialize__)($KeyboardInterrupt,$Serial$state);
  $bool (*__bool__)($KeyboardInterrupt);
  $str (*__str__)($KeyboardInterrupt);
  $str (*__repr__)($KeyboardInterrupt);
};

typedef struct $KeyboardInterrupt *$KeyboardInterrupt;

struct $KeyboardInterrupt {
  struct $KeyboardInterrupt$class *$class;
  $str error_message;
};

extern struct $KeyboardInterrupt$class $KeyboardInterrupt$methods;
$KeyboardInterrupt $KeyboardInterrupt$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $Exception$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Exception,$str);
  void (*__serialize__)($Exception,$Serial$state);
  $Exception (*__deserialize__)($Exception,$Serial$state);
  $bool (*__bool__)($Exception);
  $str (*__str__)($Exception);
  $str (*__repr__)($Exception);
};

typedef struct $Exception *$Exception;

struct $Exception {
  struct $Exception$class *$class;
  $str error_message;
};

extern struct $Exception$class $Exception$methods;
$Exception $Exception$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $AssertionError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($AssertionError,$str);
  void (*__serialize__)($AssertionError,$Serial$state);
  $AssertionError (*__deserialize__)($AssertionError,$Serial$state);
  $bool (*__bool__)($AssertionError);
  $str (*__str__)($AssertionError);
  $str (*__repr__)($AssertionError);
};

typedef struct $AssertionError *$AssertionError;

struct $AssertionError {
  struct $AssertionError$class *$class;
  $str error_message;
};

extern struct $AssertionError$class $AssertionError$methods;
$AssertionError $AssertionError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $LookupError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($LookupError,$str);
  void (*__serialize__)($LookupError,$Serial$state);
  $LookupError (*__deserialize__)($LookupError,$Serial$state);
  $bool (*__bool__)($LookupError);
  $str (*__str__)($LookupError);
  $str (*__repr__)($LookupError);
};

typedef struct $LookupError *$LookupError;

struct $LookupError {
  struct $LookupError$class *$class;
  $str error_message;
};

extern struct $LookupError$class $LookupError$methods;
$LookupError $LookupError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $IndexError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($IndexError,$str);
  void (*__serialize__)($IndexError, $Serial$state);
  $IndexError (*__deserialize__)($IndexError, $Serial$state);
  $bool (*__bool__)($IndexError);
  $str (*__str__)($IndexError);
  $str (*__repr__)($IndexError);
};

typedef struct $IndexError *$IndexError;

struct $IndexError {
  struct $IndexError$class *$class;
  $str error_message;
};

extern struct $IndexError$class $IndexError$methods;
$IndexError $IndexError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($KeyError,$str);
  void (*__serialize__)($KeyError,$Serial$state);
  $KeyError (*__deserialize__)($KeyError,$Serial$state);
  $bool (*__bool__)($KeyError);
  $str (*__str__)($KeyError);
  $str (*__repr__)($KeyError);
};

typedef struct $KeyError *$KeyError;

struct $KeyError {
  struct $KeyError$class *$class;
  $str error_message;
};

extern struct $KeyError$class $KeyError$methods;
$KeyError $KeyError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $MemoryError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($MemoryError,$str);
  void (*__serialize__)($MemoryError, $Serial$state);
  $MemoryError (*__deserialize__)($MemoryError, $Serial$state);
  $bool (*__bool__)($MemoryError);
  $str (*__str__)($MemoryError);
  $str (*__repr__)($MemoryError);
};

typedef struct $$MemoryError *$$MemoryError;

struct $MemoryError {
  struct $MemoryError$class *$class;
  $str error_message;
};

extern struct $MemoryError$class $MemoryError$methods;
$MemoryError $MemoryError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $OSError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($OSError,$str);
  void (*__serialize__)($OSError, $Serial$state);
  $OSError (*__deserialize__)($OSError, $Serial$state);
  $bool (*__bool__)($OSError);
  $str (*__str__)($OSError);
  $str (*__repr__)($OSError);
};

typedef struct $OSError *$OSError;

struct $OSError {
  struct $OSError$class *$class;
  $str error_message;
};

extern struct $OSError$class $OSError$methods;
$OSError $OSError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $RuntimeError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($RuntimeError,$str);
  void (*__serialize__)($RuntimeError, $Serial$state);
  $RuntimeError (*__deserialize__)($RuntimeError, $Serial$state);
  $bool (*__bool__)($RuntimeError);
  $str (*__str__)($RuntimeError);
  $str (*__repr__)($RuntimeError);
};

typedef struct $RuntimeError *$RuntimeError;

struct $RuntimeError {
  struct $RuntimeError$class *$class;
  $str error_message;
};

extern struct $RuntimeError$class $RuntimeError$methods;
$RuntimeError $RuntimeError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $NotImplementedError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($NotImplementedError,$str);
  void (*__serialize__)($NotImplementedError,$Serial$state);
  $NotImplementedError (*__deserialize__)($NotImplementedError,$Serial$state);
  $bool (*__bool__)($NotImplementedError);
  $str (*__str__)($NotImplementedError);
  $str (*__repr__)($NotImplementedError);
};

typedef struct $NotImplementedError *$NotImplementedError;

struct $NotImplementedError {
  struct $NotImplementedError$class *$class;
  $str error_message;
};

extern struct $NotImplementedError$class $NotImplementedError$methods;
$NotImplementedError $NotImplementedError$new($str);
////////////////////////////////////////////////////////////////////////////////////////
struct $ValueError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($ValueError,$str);
  void (*__serialize__)($ValueError, $Serial$state);
  $ValueError (*__deserialize__)($ValueError, $Serial$state);
  $bool (*__bool__)($ValueError);
  $str (*__str__)($ValueError);
  $str (*__repr__)($ValueError);
};

typedef struct $ValueError *$ValueError;

struct $ValueError {
  struct $ValueError$class *$class;
  $str error_message;
};

extern struct $ValueError$class $ValueError$methods;
$ValueError $ValueError$new($str);

void $RAISE($BaseException e);

 
/*
Exceptions hierarchy in Python 3.8 according to

https://docs.python.org/3/library/exceptions.html

BaseException
 +-- SystemExit
 +-- KeyboardInterrupt
 +-- GeneratorExit
 +-- Exception
      +-- StopIteration
      +-- StopAsyncIteration
      +-- ArithmeticError
      |    +-- FloatingPointError              ***
      |    +-- OverflowError
      |    +-- ZeroDivisionError
      +-- AssertionError
      +-- AttributeError
      +-- BufferError                          ***
      +-- EOFError
      +-- ImportError                          ***
      |    +-- ModuleNotFoundError             ***
      +-- LookupError           
      |    +-- IndexError
      |    +-- KeyError
      +-- MemoryError
      +-- NameError                            ***
      |    +-- UnboundLocalError               ***
      +-- OSError
      |    +-- BlockingIOError
      |    +-- ChildProcessError
      |    +-- ConnectionError
      |    |    +-- BrokenPipeError
      |    |    +-- ConnectionAbortedError
      |    |    +-- ConnectionRefusedError
      |    |    +-- ConnectionResetError
      |    +-- FileExistsError
      |    +-- FileNotFoundError
      |    +-- InterruptedError
      |    +-- IsADirectoryError
      |    +-- NotADirectoryError
      |    +-- PermissionError
      |    +-- ProcessLookupError
      |    +-- TimeoutError
      +-- ReferenceError                       ***
      +-- RuntimeError
      |    +-- NotImplementedError
      |    +-- RecursionError
      +-- SyntaxError                          ***
      |    +-- IndentationError
      |         +-- TabError
      +-- SystemError
      +-- TypeError                            ***
      +-- ValueError
      |    +-- UnicodeError
      |         +-- UnicodeDecodeError
      |         +-- UnicodeEncodeError
      |         +-- UnicodeTranslateError
      +-- Warning
           +-- DeprecationWarning
           +-- PendingDeprecationWarning
           +-- RuntimeWarning
           +-- SyntaxWarning
           +-- UserWarning
           +-- FutureWarning
           +-- ImportWarning
           +-- UnicodeWarning
           +-- BytesWarning
           +-- ResourceWarning
 
*/
