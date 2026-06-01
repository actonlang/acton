#pragma once

struct B_bool;
typedef struct B_bool *B_bool;

struct B_str;
typedef struct B_str *B_str;

#define B_CLOSURE_ARGS_0
#define B_CLOSURE_ARGS_1 , $WORD p0
#define B_CLOSURE_ARGS_2 , $WORD p0, $WORD p1
#define B_CLOSURE_ARGS_3 , $WORD p0, $WORD p1, $WORD p2
#define B_CLOSURE_ARGS_4 , $WORD p0, $WORD p1, $WORD p2, $WORD p3

#define B_CLOSURE_FIELDS_0
#define B_CLOSURE_FIELDS_1 $WORD p0;
#define B_CLOSURE_FIELDS_2 $WORD p0; $WORD p1;
#define B_CLOSURE_FIELDS_3 $WORD p0; $WORD p1; $WORD p2;
#define B_CLOSURE_FIELDS_4 $WORD p0; $WORD p1; $WORD p2; $WORD p3;

#define B_DECLARE_CLOSURE_CONT(N) \
struct B_ClosureCont##N; \
typedef struct B_ClosureCont##N *B_ClosureCont##N; \
typedef $R (*B_ClosureCont##N##Fn)(B_ClosureCont##N, $WORD); \
struct B_ClosureCont##N##G_class { \
    char *$GCINFO; \
    int $class_id; \
    $SuperG_class $superclass; \
    B_NoneType (*__init__)(B_ClosureCont##N, B_ClosureCont##N##Fn B_CLOSURE_ARGS_##N); \
    void (*__serialize__)(B_ClosureCont##N, $Serial$state); \
    B_ClosureCont##N (*__deserialize__)(B_ClosureCont##N, $Serial$state); \
    B_bool (*__bool__)(B_ClosureCont##N); \
    B_str (*__str__)(B_ClosureCont##N); \
    B_str (*__repr__)(B_ClosureCont##N); \
    $R (*__call__)(B_ClosureCont##N, $WORD); \
}; \
struct B_ClosureCont##N { \
    struct B_ClosureCont##N##G_class *$class; \
    B_ClosureCont##N##Fn f; \
    B_CLOSURE_FIELDS_##N \
}; \
extern struct B_ClosureCont##N##G_class B_ClosureCont##N##G_methods; \
B_ClosureCont##N B_ClosureCont##N##G_new(B_ClosureCont##N##Fn f B_CLOSURE_ARGS_##N)

#define B_DECLARE_CLOSURE_PROC(N) \
struct B_ClosureProc##N; \
typedef struct B_ClosureProc##N *B_ClosureProc##N; \
typedef $R (*B_ClosureProc##N##Fn)(B_ClosureProc##N, $Cont); \
struct B_ClosureProc##N##G_class { \
    char *$GCINFO; \
    int $class_id; \
    $SuperG_class $superclass; \
    B_NoneType (*__init__)(B_ClosureProc##N, B_ClosureProc##N##Fn B_CLOSURE_ARGS_##N); \
    void (*__serialize__)(B_ClosureProc##N, $Serial$state); \
    B_ClosureProc##N (*__deserialize__)(B_ClosureProc##N, $Serial$state); \
    B_bool (*__bool__)(B_ClosureProc##N); \
    B_str (*__str__)(B_ClosureProc##N); \
    B_str (*__repr__)(B_ClosureProc##N); \
    $R (*__call__)(B_ClosureProc##N, $Cont); \
    $R (*__exec__)(B_ClosureProc##N, $Cont); \
}; \
struct B_ClosureProc##N { \
    struct B_ClosureProc##N##G_class *$class; \
    B_ClosureProc##N##Fn f; \
    B_CLOSURE_FIELDS_##N \
}; \
extern struct B_ClosureProc##N##G_class B_ClosureProc##N##G_methods; \
B_ClosureProc##N B_ClosureProc##N##G_new(B_ClosureProc##N##Fn f B_CLOSURE_ARGS_##N)

B_DECLARE_CLOSURE_CONT(0);
B_DECLARE_CLOSURE_CONT(1);
B_DECLARE_CLOSURE_CONT(2);
B_DECLARE_CLOSURE_CONT(3);
B_DECLARE_CLOSURE_CONT(4);

B_DECLARE_CLOSURE_PROC(0);
B_DECLARE_CLOSURE_PROC(1);
B_DECLARE_CLOSURE_PROC(2);
B_DECLARE_CLOSURE_PROC(3);
B_DECLARE_CLOSURE_PROC(4);

struct B_ClosureMut0;
typedef struct B_ClosureMut0 *B_ClosureMut0;
typedef $WORD (*B_ClosureMut0Fn)(B_ClosureMut0);
struct B_ClosureMut0G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_ClosureMut0, B_ClosureMut0Fn);
    void (*__serialize__)(B_ClosureMut0, $Serial$state);
    B_ClosureMut0 (*__deserialize__)(B_ClosureMut0, $Serial$state);
    B_bool (*__bool__)(B_ClosureMut0);
    B_str (*__str__)(B_ClosureMut0);
    B_str (*__repr__)(B_ClosureMut0);
    $R (*__call__)(B_ClosureMut0, $Cont);
    $R (*__exec__)(B_ClosureMut0, $Cont);
    $WORD (*__eval__)(B_ClosureMut0);
};
struct B_ClosureMut0 {
    struct B_ClosureMut0G_class *$class;
    B_ClosureMut0Fn f;
};
extern struct B_ClosureMut0G_class B_ClosureMut0G_methods;
B_ClosureMut0 B_ClosureMut0G_new(B_ClosureMut0Fn f);

void B_ClosureQ___init__();

#undef B_DECLARE_CLOSURE_CONT
#undef B_DECLARE_CLOSURE_PROC
#undef B_CLOSURE_ARGS_0
#undef B_CLOSURE_ARGS_1
#undef B_CLOSURE_ARGS_2
#undef B_CLOSURE_ARGS_3
#undef B_CLOSURE_ARGS_4
#undef B_CLOSURE_FIELDS_0
#undef B_CLOSURE_FIELDS_1
#undef B_CLOSURE_FIELDS_2
#undef B_CLOSURE_FIELDS_3
#undef B_CLOSURE_FIELDS_4
