#pragma once

B_WorldCap B_WorldCapG_new();
B_NoneType B_WorldCapD___init__ (B_WorldCap self);

// Fill buf with len secure random bytes from the per-thread generator behind
// env.random, or raise OSError if the operating system cannot seed it. Defined
// in __builtin__.ext.c.
void B_EnvRandom_fill(void *buf, size_t len);
