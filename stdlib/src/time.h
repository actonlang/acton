#pragma once
#include "builtin/builtin.h"
#include "builtin/minienv.h"
#include "rts/rts.h"
$float time$$monotonic ();
$int time$$monotonic_ns ();
$float time$$time ();
$int time$$time_ns ();
void time$$__init__ ();
