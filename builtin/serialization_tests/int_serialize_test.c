#include "../builtin.h"
#include <stdio.h>

int main() {
    $register_builtin();
    $int n = $int$new(($atom)to$str("12345678909876543212345678987654321"));
    $ROW r = $serialize(($Serializable)n,NULL);
    $int n1 = ($int)$deserialize(r,NULL);
    printf("%s\n",n1->$class->__str__(n1)->str);
}
