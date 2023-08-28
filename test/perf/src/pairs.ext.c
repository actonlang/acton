#include "rts/log.h"
#include "rts/rts.h"

void pairsQ___ext_init__ () {
}

$R pairsQ_NodeD_set_affinityG_local (pairsQ_Node self, $Cont C_cont, B_int wthread) {
    set_actor_affinity(from$int(wthread));
    return $RU_CONT(C_cont, B_None);
}
