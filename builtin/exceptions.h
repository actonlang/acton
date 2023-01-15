struct B_BaseExceptionG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_BaseException,B_str);
    void (*__serialize__)(B_BaseException, $Serial$state);
    B_BaseException (*__deserialize__)(B_BaseException, $Serial$state);
    B_bool (*__bool__)(B_BaseException);
    B_str (*__str__)(B_BaseException);
    B_str (*__repr__)(B_BaseException);
};

typedef struct B_BaseException *B_BaseException;

struct B_BaseException {
    struct B_BaseExceptionG_class *$class;
    B_str error_message;
};

extern struct B_BaseExceptionG_class B_BaseExceptionG_methods;
B_BaseException B_BaseExceptionG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_SystemExitG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_SystemExit,B_str);
    void (*__serialize__)(B_SystemExit,$Serial$state);
    B_SystemExit (*__deserialize__)(B_SystemExit,$Serial$state);
    B_bool (*__bool__)(B_SystemExit);
    B_str (*__str__)(B_SystemExit);
    B_str (*__repr__)(B_SystemExit);
};

typedef struct B_SystemExit *B_SystemExit;

struct B_SystemExit {
    struct B_SystemExitG_class *$class;
    B_str error_message;
};

extern struct B_SystemExitG_class B_SystemExitG_methods;
B_SystemExit B_SystemExitG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_KeyboardInterruptG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_KeyboardInterrupt,B_str);
    void (*__serialize__)(B_KeyboardInterrupt,$Serial$state);
    B_KeyboardInterrupt (*__deserialize__)(B_KeyboardInterrupt,$Serial$state);
    B_bool (*__bool__)(B_KeyboardInterrupt);
    B_str (*__str__)(B_KeyboardInterrupt);
    B_str (*__repr__)(B_KeyboardInterrupt);
};

typedef struct B_KeyboardInterrupt *B_KeyboardInterrupt;

struct B_KeyboardInterrupt {
    struct B_KeyboardInterruptG_class *$class;
    B_str error_message;
};

extern struct B_KeyboardInterruptG_class B_KeyboardInterruptG_methods;
B_KeyboardInterrupt B_KeyboardInterruptG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_ExceptionG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Exception,B_str);
    void (*__serialize__)(B_Exception,$Serial$state);
    B_Exception (*__deserialize__)(B_Exception,$Serial$state);
    B_bool (*__bool__)(B_Exception);
    B_str (*__str__)(B_Exception);
    B_str (*__repr__)(B_Exception);
};

typedef struct B_Exception *B_Exception;

struct B_Exception {
    struct B_ExceptionG_class *$class;
    B_str error_message;
};

extern struct B_ExceptionG_class B_ExceptionG_methods;
B_Exception B_ExceptionG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_AssertionErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_AssertionError,B_str);
    void (*__serialize__)(B_AssertionError,$Serial$state);
    B_AssertionError (*__deserialize__)(B_AssertionError,$Serial$state);
    B_bool (*__bool__)(B_AssertionError);
    B_str (*__str__)(B_AssertionError);
    B_str (*__repr__)(B_AssertionError);
};

typedef struct B_AssertionError *B_AssertionError;

struct B_AssertionError {
    struct B_AssertionErrorG_class *$class;
    B_str error_message;
};

extern struct B_AssertionErrorG_class B_AssertionErrorG_methods;
B_AssertionError B_AssertionErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_LookupErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_LookupError,B_str);
    void (*__serialize__)(B_LookupError,$Serial$state);
    B_LookupError (*__deserialize__)(B_LookupError,$Serial$state);
    B_bool (*__bool__)(B_LookupError);
    B_str (*__str__)(B_LookupError);
    B_str (*__repr__)(B_LookupError);
};

typedef struct B_LookupError *B_LookupError;

struct B_LookupError {
    struct B_LookupErrorG_class *$class;
    B_str error_message;
};

extern struct B_LookupErrorG_class B_LookupErrorG_methods;
B_LookupError B_LookupErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_IndexErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IndexError,B_str);
    void (*__serialize__)(B_IndexError, $Serial$state);
    B_IndexError (*__deserialize__)(B_IndexError, $Serial$state);
    B_bool (*__bool__)(B_IndexError);
    B_str (*__str__)(B_IndexError);
    B_str (*__repr__)(B_IndexError);
};

typedef struct B_IndexError *B_IndexError;

struct B_IndexError {
    struct B_IndexErrorG_class *$class;
    B_str error_message;
};

extern struct B_IndexErrorG_class B_IndexErrorG_methods;
B_IndexError B_IndexErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_KeyErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_KeyError,B_str);
    void (*__serialize__)(B_KeyError,$Serial$state);
    B_KeyError (*__deserialize__)(B_KeyError,$Serial$state);
    B_bool (*__bool__)(B_KeyError);
    B_str (*__str__)(B_KeyError);
    B_str (*__repr__)(B_KeyError);
};

typedef struct B_KeyError *B_KeyError;

struct B_KeyError {
    struct B_KeyErrorG_class *$class;
    B_str error_message;
};

extern struct B_KeyErrorG_class B_KeyErrorG_methods;
B_KeyError B_KeyErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_MemoryErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_MemoryError,B_str);
    void (*__serialize__)(B_MemoryError, $Serial$state);
    B_MemoryError (*__deserialize__)(B_MemoryError, $Serial$state);
    B_bool (*__bool__)(B_MemoryError);
    B_str (*__str__)(B_MemoryError);
    B_str (*__repr__)(B_MemoryError);
};

typedef struct $B_MemoryError *$B_MemoryError;

struct B_MemoryError {
    struct B_MemoryErrorG_class *$class;
    B_str error_message;
};

extern struct B_MemoryErrorG_class B_MemoryErrorG_methods;
B_MemoryError B_MemoryErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_OSErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_OSError,B_str);
    void (*__serialize__)(B_OSError, $Serial$state);
    B_OSError (*__deserialize__)(B_OSError, $Serial$state);
    B_bool (*__bool__)(B_OSError);
    B_str (*__str__)(B_OSError);
    B_str (*__repr__)(B_OSError);
};

typedef struct B_OSError *B_OSError;

struct B_OSError {
    struct B_OSErrorG_class *$class;
    B_str error_message;
};

extern struct B_OSErrorG_class B_OSErrorG_methods;
B_OSError B_OSErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_RuntimeErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_RuntimeError,B_str);
    void (*__serialize__)(B_RuntimeError, $Serial$state);
    B_RuntimeError (*__deserialize__)(B_RuntimeError, $Serial$state);
    B_bool (*__bool__)(B_RuntimeError);
    B_str (*__str__)(B_RuntimeError);
    B_str (*__repr__)(B_RuntimeError);
};

typedef struct B_RuntimeError *B_RuntimeError;

struct B_RuntimeError {
    struct B_RuntimeErrorG_class *$class;
    B_str error_message;
};

extern struct B_RuntimeErrorG_class B_RuntimeErrorG_methods;
B_RuntimeError B_RuntimeErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_NotImplementedErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_NotImplementedError,B_str);
    void (*__serialize__)(B_NotImplementedError,$Serial$state);
    B_NotImplementedError (*__deserialize__)(B_NotImplementedError,$Serial$state);
    B_bool (*__bool__)(B_NotImplementedError);
    B_str (*__str__)(B_NotImplementedError);
    B_str (*__repr__)(B_NotImplementedError);
};

typedef struct B_NotImplementedError *B_NotImplementedError;

struct B_NotImplementedError {
    struct B_NotImplementedErrorG_class *$class;
    B_str error_message;
};

extern struct B_NotImplementedErrorG_class B_NotImplementedErrorG_methods;
B_NotImplementedError B_NotImplementedErrorG_new(B_str);
////////////////////////////////////////////////////////////////////////////////////////
struct B_ValueErrorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_ValueError,B_str);
    void (*__serialize__)(B_ValueError, $Serial$state);
    B_ValueError (*__deserialize__)(B_ValueError, $Serial$state);
    B_bool (*__bool__)(B_ValueError);
    B_str (*__str__)(B_ValueError);
    B_str (*__repr__)(B_ValueError);
};

typedef struct B_ValueError *B_ValueError;

struct B_ValueError {
    struct B_ValueErrorG_class *$class;
    B_str error_message;
};

extern struct B_ValueErrorG_class B_ValueErrorG_methods;
B_ValueError B_ValueErrorG_new(B_str);

void $RAISE(B_BaseException e);

 
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
