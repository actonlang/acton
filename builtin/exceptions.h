struct $BaseException$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($BaseException,$str);
  void (*__serialize__)($BaseException, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $BaseException (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $BaseException {
  struct $BaseException$class *$class;
  $str error_message;
};

extern struct $BaseException$class $BaseException$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $SystemExit$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($SystemExit,$str);
  void (*__serialize__)($SystemExit, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $SystemExit (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $SystemExit {
  struct $SystemExit$class *$class;
  $str error_message;
};

extern struct $SystemExit$class $SystemExit$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyboardInterrupt$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($KeyboardInterrupt,$str);
  void (*__serialize__)($KeyboardInterrupt, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $KeyboardInterrupt (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $KeyboardInterrupt {
  struct $KeyboardInterrupt$class *$class;
  $str error_message;
};

extern struct $KeyboardInterrupt$class $KeyboardInterrupt$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $Exception$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($Exception,$str);
  void (*__serialize__)($Exception, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $Exception (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $Exception {
  struct $Exception$class *$class;
  $str error_message;
};

extern struct $Exception$class $Exception$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $AssertionError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($AssertionError,$str);
  void (*__serialize__)($AssertionError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $AssertionError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $AssertionError {
  struct $AssertionError$class *$class;
  $str error_message;
};

extern struct $AssertionError$class $AssertionError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $LookupError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($LookupError,$str);
  void (*__serialize__)($LookupError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $LookupError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $LookupError {
  struct $LookupError$class *$class;
  $str error_message;
};

extern struct $LookupError$class $LookupError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $IndexError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($IndexError,$str);
  void (*__serialize__)($IndexError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $IndexError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $IndexError {
  struct $IndexError$class *$class;
  $str error_message;
};

extern struct $IndexError$class $IndexError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $KeyError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($KeyError,$str);
  void (*__serialize__)($KeyError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $KeyError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $KeyError {
  struct $KeyError$class *$class;
  $str error_message;
};

extern struct $KeyError$class $KeyError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $MemoryError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($MemoryError,$str);
  void (*__serialize__)($MemoryError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $MemoryError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $MemoryError {
  struct $MemoryError$class *$class;
  $str error_message;
};

extern struct $MemoryError$class $MemoryError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $OSError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($OSError,$str);
  void (*__serialize__)($OSError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $OSError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $OSError {
  struct $OSError$class *$class;
  $str error_message;
};

extern struct $OSError$class $OSError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $RuntimeError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($RuntimeError,$str);
  void (*__serialize__)($RuntimeError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $RuntimeError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $RuntimeError {
  struct $RuntimeError$class *$class;
  $str error_message;
};

extern struct $RuntimeError$class $RuntimeError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $NotImplementedError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($NotImplementedError,$str);
  void (*__serialize__)($NotImplementedError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $NotImplementedError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $NotImplementedError {
  struct $NotImplementedError$class *$class;
  $str error_message;
};

extern struct $NotImplementedError$class $NotImplementedError$methods;
////////////////////////////////////////////////////////////////////////////////////////
struct $ValueError$class {
  char *$GCINFO;
  $Super$class $superclass;
  void (*__init__)($ValueError,$str);
  void (*__serialize__)($ValueError, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
  $ValueError (*__deserialize__)($Mapping$dict, $ROW*, $dict);
};

struct $ValueError {
  struct $ValueError$class *$class;
  $str error_message;
};

extern struct $ValueError$class $ValueError$methods;
 



 
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
