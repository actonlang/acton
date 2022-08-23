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
