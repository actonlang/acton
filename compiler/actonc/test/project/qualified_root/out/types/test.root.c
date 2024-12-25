#include "rts/common.h"
#include "out/types/test.h"
void $ROOTINIT () {
    testQ___init__();
}
$Actor $ROOT () {
    return ($Actor)$NEWACTOR(testQ_main);
}