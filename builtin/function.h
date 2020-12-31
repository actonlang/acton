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
    $WORD (*__call__)($function, ...);
};
struct $function {
    struct $function$class *$class;
};
