#ifndef hash_h
#define hash_h

#include <stdint.h>

#define HASH_POINTERSHIFT 3
#define HASH_MAGIC 2654435761

static inline uintptr_t hash_address(const void* p)
{
  return ((uintptr_t)p >> HASH_POINTERSHIFT) * HASH_MAGIC;
}

#endif
