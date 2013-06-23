#include "pool.h"
#include <sys/mman.h>

#define POOL_SIZE (1 << 16)
#define POOL_MIN 64
#define CACHE_LINE 64

typedef struct pool_local_t
{
  void* next;
} pool_local_t;

typedef struct pool_central_t
{
  void* next;
  uintptr_t len;
  struct pool_central_t* central;
} pool_central_t;

typedef struct pool_cmp_t
{
  union
  {
    struct
    {
      uint64_t aba;
      pool_central_t* node;
    };

    pool_aba_t dw;
  };
} pool_cmp_t;

static size_t next_size(size_t size)
{
  if(size <= POOL_MIN) { return POOL_MIN; }

  size_t n = size / CACHE_LINE;
  n *= CACHE_LINE;

  if(n < size) { n += CACHE_LINE; }

  return n;
}

static void pool_push(void** pool, size_t* len, pool_aba_t* central)
{
  pool_cmp_t cmp, xchg;
  pool_central_t* t = *pool;
  t->len = *len;

  do
  {
    cmp.dw = *central;
    t->central = cmp.node;
    xchg.node = t;
    xchg.aba = cmp.aba + 1;
  } while(!__sync_bool_compare_and_swap(central, cmp.dw, xchg.dw));

  *pool = NULL;
  *len = 0;
}

static bool pool_pull(void** pool, size_t* len, void** start, void** end,
  pool_aba_t* central)
{
  pool_cmp_t cmp, xchg;
  pool_central_t* next;

  do
  {
    cmp.dw = *central;
    next = cmp.node;

    if(next == NULL)
    {
      return false;
    }

    xchg.node = cmp.node->central;
    xchg.aba = cmp.aba + 1;
  } while(!__sync_bool_compare_and_swap(central, cmp.dw, xchg.dw));

  *pool = next;
  *len = next->len;

  return true;
}

static void* pool_get(void** pool, size_t* len, void** start, void** end,
  pool_aba_t* central, size_t size)
{
  pool_local_t* t = *pool;

  if(t != NULL)
  {
    *pool = t->next;
    *len = *len - 1;
  } else if(*start < *end) {
    t = *start;
    *start = *start + size;
  } else if(pool_pull(pool, len, start, end, central)) {
    return pool_get(pool, len, start, end, central, size);
  }

  return t;
}

static void* pool_pages(void** start, void** end, size_t size, size_t count)
{
  pool_local_t* t = mmap(
    0,
    POOL_SIZE,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANON,
    -1,
    0
    );

  if(t == MAP_FAILED) { abort(); }

  *start = ((void*)t) + size;
  *end = ((void*)t) + (size * count);

  return t;
}

void pool_init(size_t in, size_t* size, size_t* count)
{
  *size = next_size(in);
  *count = POOL_SIZE / *size;

  if(*count == 0) { abort(); }
}

void* pool_alloc(void** pool, size_t* len, void** start, void** end,
  pool_aba_t* central, size_t size, size_t count)
{
  pool_local_t* t = pool_get(pool, len, start, end, central, size);

  if(t == NULL)
  {
    t = pool_pages(start, end, size, count);
  }

  return t;
}

void pool_free(void* t, void** pool, size_t* len, pool_aba_t* central,
  size_t count)
{
  if(*len >= count)
  {
    pool_push(pool, len, central);
  }

  pool_local_t* lt = t;
  lt->next = *pool;
  *pool = t;
  *len = *len + 1;
}
