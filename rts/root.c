#include "pingpong.h"

$R $ROOT(int env, $Cont then) {
    return $NEWCC(Pingpong, then, env);
}
