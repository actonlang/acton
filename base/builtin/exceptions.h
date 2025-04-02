#include <setjmp.h>

struct JumpBuf;
typedef struct JumpBuf *JumpBuf;
struct JumpBuf {
    jmp_buf buf;
    B_BaseException xval;
    JumpBuf prev;
};

void $RAISE(B_BaseException e);
JumpBuf $PUSH_BUF();
void $DROP();
B_BaseException $POP();
#define $PUSH()             (!setjmp($PUSH_BUF()->buf))

 
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

  Plus the (hidden) primitive exceptions that implement control flow in the presence of finalizers:

  +-- $SEQ
  +-- $BRK
  +-- $CNT
  +-- $RET
 
*/

struct $SEQ;
struct $BRK;
struct $CNT;
struct $RET;

typedef struct $SEQ *$SEQ;
typedef struct $BRK *$BRK;
typedef struct $CNT *$CNT;
typedef struct $RET *$RET;

struct $SEQG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($SEQ);
    void (*__serialize__) ($SEQ, $Serial$state);
    $SEQ (*__deserialize__) ($SEQ, $Serial$state);
};
struct $SEQ {
    struct $SEQG_class *$class;
    B_str error_message;
};
struct $BRKG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($BRK);
    void (*__serialize__) ($BRK, $Serial$state);
    $BRK (*__deserialize__) ($BRK, $Serial$state);
};
struct $BRK {
    struct $BRKG_class *$class;
    B_str error_message;
};
struct $CNTG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($CNT);
    void (*__serialize__) ($CNT, $Serial$state);
    $CNT (*__deserialize__) ($CNT, $Serial$state);
};
struct $CNT {
    struct $CNTG_class *$class;
    B_str error_message;
};
struct $RETG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) ($RET, B_value);
    void (*__serialize__) ($RET, $Serial$state);
    $RET (*__deserialize__) ($RET, $Serial$state);
};
struct $RET {
    struct $RETG_class *$class;
    B_str error_message;
    B_value val;
};

extern struct $SEQG_class $SEQG_methods;
extern struct $BRKG_class $BRKG_methods;
extern struct $CNTG_class $CNTG_methods;
extern struct $RETG_class $RETG_methods;

$SEQ $SEQG_new();
$BRK $BRKG_new();
$CNT $CNTG_new();
$RET $RETG_new(B_value);
