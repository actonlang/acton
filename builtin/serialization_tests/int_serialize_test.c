#include "../builtin.h"
#include <stdio.h>

int main() {
    $register_builtin();
    B_int n = B_intG_new((B_atom)to$str("12345678909876543212345678987654321"));
    $ROW r = $serialize(($Serializable)n,NULL);
    B_int n1 = (B_int)$deserialize(r,NULL);
    printf("%s\n",n1->$class->__str__(n1)->str);
}
