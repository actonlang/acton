struct $float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($float, $atom);
    void (*__serialize__)($float,$Serial$state);
    $float (*__deserialize__)($float,$Serial$state);
    $bool (*__bool__)($float);
    $str (*__str__)($float);
    $str (*__repr__)($float);
};


struct $float {
    struct $float$class *$class;
    double val;
};

extern struct $float$class $float$methods;
$float $float$new($atom);

extern struct $Real$float$class $Real$float$methods;
$Real$float $Real$float$new();

#define $RealFloat$float$new(...) $Real$float$new(__VA_ARGS__)
#define $RealFloat$float $Real$float

extern struct $Div$float$class $Div$float$methods;
$Div$float $Div$float$new();

extern struct $Minus$float$class $Minus$float$methods;
$Minus$float $Minus$float$new($Real);

extern struct $Ord$float$class $Ord$float$methods;
$Ord$float $Ord$float$new();
extern struct $Hashable$float$class $Hashable$float$methods;
$Hashable$float $Hashable$float$new();

extern struct $Real$float *$Real$float$witness;
extern struct $Minus$float *$Minus$float$witness;
extern struct $Ord$float *$Ord$float$witness;
extern struct $Hashable$float *$Hashable$float$witness;

$float to$float(double x);
double from$float($float x);

$float $float$new($atom a);
