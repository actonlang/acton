struct $BaseException$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($BaseException,$str);
  $bool (*__bool__)($BaseException);
  $str (*__str__)($BaseException);
  void (*__serialize__)($BaseException, $Serial$state);
  $BaseException (*__deserialize__)($Serial$state);
};

typedef struct $BaseException *$BaseException;

struct $BaseException {
  struct $BaseException$class *$class;
  $str error_message;
};

extern struct $BaseException$class $BaseException$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $SystemExit$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($SystemExit,$str);
  $bool (*__bool__)($SystemExit);
  $str (*__str__)($SystemExit);
  void (*__serialize__)($SystemExit,$Serial$state);
  $SystemExit (*__deserialize__)($Serial$state);
};

typedef struct $SystemExit *$SystemExit;

struct $SystemExit {
  struct $SystemExit$class *$class;
  $str error_message;
};

extern struct $SystemExit$class $SystemExit$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyboardInterrupt$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($KeyboardInterrupt,$str);
  $bool (*__bool__)($KeyboardInterrupt);
  $str (*__str__)($KeyboardInterrupt);
  void (*__serialize__)($KeyboardInterrupt,$Serial$state);
  $KeyboardInterrupt (*__deserialize__)($Serial$state);
};

typedef struct $KeyboardInterrupt *$KeyboardInterrupt;

struct $KeyboardInterrupt {
  struct $KeyboardInterrupt$class *$class;
  $str error_message;
};

extern struct $KeyboardInterrupt$class $KeyboardInterrupt$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $Exception$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Exception,$str);
  $bool (*__bool__)($Exception);
  $str (*__str__)($Exception);
  void (*__serialize__)($Exception,$Serial$state);
  $Exception (*__deserialize__)($Serial$state);
};

typedef struct $Exception *$Exception;

struct $Exception {
  struct $Exception$class *$class;
  $str error_message;
};

extern struct $Exception$class $Exception$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $AssertionError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($AssertionError,$str);
  $bool (*__bool__)($AssertionError);
  $str (*__str__)($AssertionError);
  void (*__serialize__)($AssertionError,$Serial$state);
  $AssertionError (*__deserialize__)($Serial$state);
};

typedef struct $AssertionError *$AssertionError;

struct $AssertionError {
  struct $AssertionError$class *$class;
  $str error_message;
};

extern struct $AssertionError$class $AssertionError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $LookupError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($LookupError,$str);
  $bool (*__bool__)($LookupError);
  $str (*__str__)($LookupError);
  void (*__serialize__)($LookupError,$Serial$state);
  $LookupError (*__deserialize__)($Serial$state);
};

typedef struct $LookupError *$LookupError;

struct $LookupError {
  struct $LookupError$class *$class;
  $str error_message;
};

extern struct $LookupError$class $LookupError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $IndexError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($IndexError,$str);
  $bool (*__bool__)($IndexError);
  $str (*__str__)($IndexError);
  void (*__serialize__)($IndexError, $Serial$state);
  $IndexError (*__deserialize__)($Serial$state);
};

typedef struct $IndexError *$IndexError;

struct $IndexError {
  struct $IndexError$class *$class;
  $str error_message;
};

extern struct $IndexError$class $IndexError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($KeyError,$str);
  $bool (*__bool__)($KeyError);
  $str (*__str__)($KeyError);
  void (*__serialize__)($KeyError,$Serial$state);
  $KeyError (*__deserialize__)($Serial$state);
};

typedef struct $KeyError *$KeyError;

struct $KeyError {
  struct $KeyError$class *$class;
  $str error_message;
};

extern struct $KeyError$class $KeyError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $MemoryError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($MemoryError,$str);
  $bool (*__bool__)($MemoryError);
  $str (*__str__)($MemoryError);
  void (*__serialize__)($MemoryError, $Serial$state);
  $MemoryError (*__deserialize__)($Serial$state);
};

typedef struct $$MemoryError *$$MemoryError;

struct $MemoryError {
  struct $MemoryError$class *$class;
  $str error_message;
};

extern struct $MemoryError$class $MemoryError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $OSError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($OSError,$str);
  $bool (*__bool__)($OSError);
  $str (*__str__)($OSError);
  void (*__serialize__)($OSError, $Serial$state);
  $OSError (*__deserialize__)($Serial$state);
};

typedef struct $OSError *$OSError;

struct $OSError {
  struct $OSError$class *$class;
  $str error_message;
};

extern struct $OSError$class $OSError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $RuntimeError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($RuntimeError,$str);
  $bool (*__bool__)($RuntimeError);
  $str (*__str__)($RuntimeError);
  void (*__serialize__)($RuntimeError, $Serial$state);
  $RuntimeError (*__deserialize__)($Serial$state);
};

typedef struct $RuntimeError *$RuntimeError;

struct $RuntimeError {
  struct $RuntimeError$class *$class;
  $str error_message;
};

extern struct $RuntimeError$class $RuntimeError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $NotImplementedError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($NotImplementedError,$str);
  $bool (*__bool__)($NotImplementedError);
  $str (*__str__)($NotImplementedError);
  void (*__serialize__)($NotImplementedError,$Serial$state);
  $NotImplementedError (*__deserialize__)($Serial$state);
};

typedef struct $NotImplementedError *$NotImplementedError;

struct $NotImplementedError {
  struct $NotImplementedError$class *$class;
  $str error_message;
};

extern struct $NotImplementedError$class $NotImplementedError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $ValueError$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($ValueError,$str);
  $bool (*__bool__)($ValueError);
  $str (*__str__)($ValueError);
  void (*__serialize__)($ValueError, $Serial$state);
  $ValueError (*__deserialize__)($Serial$state);
};

typedef struct $ValueError *$ValueError;

struct $ValueError {
  struct $ValueError$class *$class;
  $str error_message;
};

extern struct $ValueError$class $ValueError$methods;

void RAISE($BaseException e);

 
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
