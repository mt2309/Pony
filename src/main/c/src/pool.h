#ifndef pool_h
#define pool_h

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

typedef volatile __int128_t pool_aba_t;

void pool_init(size_t in, size_t* size, size_t* count);

void* pool_alloc(void** pool, size_t* len, void** start, void** end,
  pool_aba_t* central, size_t size, size_t count);

void pool_free(void* t, void** pool, size_t* len, pool_aba_t* central, size_t count);

#define POOL_CREATE(TYPE) \
  __thread void* pool_##TYPE; \
  __thread size_t pool_length_##TYPE; \
  __thread void* pool_start_##TYPE; \
  __thread void* pool_end_##TYPE; \
  size_t pool_size_##TYPE; \
  size_t pool_count_##TYPE; \
  pool_aba_t pool_central_##TYPE; \
  \
  __attribute__ ((constructor)) static void pool_init_##TYPE() \
  { \
    pool_init(sizeof(TYPE), &pool_size_##TYPE, &pool_count_##TYPE); \
  }

#define POOL_REFERENCE(TYPE) \
  extern __thread void* pool_##TYPE; \
  extern __thread size_t pool_length_##TYPE; \
  extern __thread void* pool_start_##TYPE; \
  extern __thread void* pool_end_##TYPE; \
  extern size_t pool_size_##TYPE; \
  extern size_t pool_count_##TYPE; \
  extern pool_aba_t pool_central_##TYPE;

#define POOL_ALLOC(TYPE) \
  pool_alloc(&pool_##TYPE, &pool_length_##TYPE, \
    &pool_start_##TYPE, &pool_end_##TYPE, \
    &pool_central_##TYPE, pool_size_##TYPE, pool_count_##TYPE)

#define POOL_FREE(TYPE, VALUE) \
  pool_free( (void*)VALUE, &pool_##TYPE, &pool_length_##TYPE, \
    &pool_central_##TYPE, pool_count_##TYPE )

#endif
