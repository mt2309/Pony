#ifndef cpu_h
#define cpu_h

#include <stdint.h>
#include <stdbool.h>

uint32_t cpu_count();

bool cpu_hyperthread(uint32_t cpu);

void cpu_affinity(uint32_t cpu);

#endif
