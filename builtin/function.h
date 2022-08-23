#pragma once

struct $function;
typedef struct $function *$function;
struct $function$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($function);
    void (*__serialize__)($function, $Serial$state);
    $function (*__deserialize__)($function, $Serial$state);
    $bool (*__bool__)($function);
    $str (*__str__)($function);
    $str (*__repr__)($function);
    $WORD (*__call__)($function);
};
struct $function {
    struct $function$class *$class;
};
extern struct $function$class $function$methods;


struct $function1;
typedef struct $function1 *$function1;
struct $function1$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($function1);
    void (*__serialize__)($function1, $Serial$state);
    $function1 (*__deserialize__)($function1, $Serial$state);
    $bool (*__bool__)($function1);
    $str (*__str__)($function1);
    $str (*__repr__)($function1);
    $WORD (*__call__)($function1, $WORD);
};
struct $function1 {
    struct $function1$class *$class;
};

struct $function2;
typedef struct $function2 *$function2;
struct $function2$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($function2);
    void (*__serialize__)($function2, $Serial$state);
    $function2 (*__deserialize__)($function2, $Serial$state);
    $bool (*__bool__)($function2);
    $str (*__str__)($function2);
    $str (*__repr__)($function2);
    $WORD (*__call__)($function2, $WORD, $WORD);
};
struct $function2 {
    struct $function2$class *$class;
};

struct $function3;
typedef struct $function3 *$function3;
struct $function3$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($function3);
    void (*__serialize__)($function3, $Serial$state);
    $function3 (*__deserialize__)($function3, $Serial$state);
    $bool (*__bool__)($function3);
    $str (*__str__)($function3);
    $str (*__repr__)($function3);
    $WORD (*__call__)($function3, $WORD, $WORD, $WORD);
};
struct $function3 {
    struct $function3$class *$class;
};
