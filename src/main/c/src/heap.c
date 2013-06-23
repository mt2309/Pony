#include "heap.h"
#include "pool.h"
#include "pagemap.h"
#include <string.h>
#include <sys/mman.h>

#define HEAP_INITIALGC (1 << 14)
#define HEAP_DROPGC (1 << 8);

#define HEAP_MIN (1 << HEAP_MINBITS)
#define HEAP_MAX (1 << HEAP_MAXBITS)

#define HEAP_PAGESIZE (1 << 12)
#define HEAP_PAGEMASK (~(HEAP_PAGESIZE - 1))

struct chunk_t
{
  actor_t* actor;
  void* m;

  bool large;
  bool large_mark;

  union
  {
    struct
    {
      uint32_t sizeclass;
      uint32_t slots;
    };

    uint64_t len;
  };

  struct chunk_t* next;
};

typedef char block_t[HEAP_MAX];
typedef void (*chunk_fn_t)(chunk_t* chunk);

POOL_CREATE(chunk_t);
POOL_CREATE(block_t);

static const uint8_t sizeclass_table[HEAP_MAX / HEAP_MIN] =
{
  0, 1, 2, 2, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5, 5,
};

static const uint32_t sizeclass_empty[HEAP_SIZECLASSES] =
{
  0xFFFFFFFF,
  0x55555555,
  0x11111111,
  0x01010101,
  0x00010001,
  0x00000001
};

static void clear_small(chunk_t* chunk)
{
  chunk->slots = sizeclass_empty[chunk->sizeclass];
}

static void clear_large(chunk_t* chunk)
{
  chunk->large_mark = false;
}

static void destroy_small(chunk_t* chunk)
{
  POOL_FREE(block_t, chunk->m);
  POOL_FREE(chunk_t, chunk);
}

static void destroy_large(chunk_t* chunk)
{
  munmap(chunk->m, chunk->len);
  POOL_FREE(chunk_t, chunk);
}

static size_t sweep_small(chunk_t* chunk, chunk_t** avail, chunk_t** full,
  uint32_t empty)
{
  size_t used = 0;
  chunk_t* next;

  while(chunk != NULL)
  {
    next = chunk->next;

    if(chunk->slots == 0)
    {
      used += sizeof(block_t);
      chunk->next = *full;
      *full = chunk;
    } else if(chunk->slots == empty) {
      destroy_small(chunk);
    } else {
      used += sizeof(block_t) -
        (__builtin_popcount(chunk->slots) << (chunk->sizeclass + HEAP_MINBITS));
      chunk->next = *avail;
      *avail = chunk;
    }

    chunk = next;
  }

  return used;
}

static chunk_t* sweep_large(chunk_t* chunk, size_t* used)
{
  chunk_t* list = NULL;
  chunk_t* next;

  while(chunk != NULL)
  {
    next = chunk->next;

    if(chunk->large_mark)
    {
      chunk->large_mark = false;
      chunk->next = list;
      list = chunk;
      *used += chunk->len;
    } else {
      destroy_large(chunk);
    }

    chunk = next;
  }

  return list;
}

static void chunk_list(chunk_fn_t f, chunk_t* current)
{
  chunk_t* next;

  while(current != NULL)
  {
    next = current->next;
    f(current);
    current = next;
  }
}

static uint8_t small_sizeclass(size_t size)
{
  // size is in range 1..HEAP_MAX
  // change to 0..((HEAP_MAX / HEAP_MIN) - 1) and look up in table
  return sizeclass_table[(size - 1) >> HEAP_MINBITS];
}

static void* small_malloc(actor_t* actor, heap_t* heap, size_t size)
{
  uint32_t sizeclass = small_sizeclass(size);
  chunk_t* chunk = heap->small[sizeclass];

  // if there are none in this size class, get a new one
  if(chunk == NULL)
  {
    chunk_t* n = POOL_ALLOC(chunk_t);
    n->actor = actor;
    n->m = POOL_ALLOC(block_t);
    n->large = false;
    n->large_mark = false;
    n->sizeclass = sizeclass;
    n->slots = sizeclass_empty[sizeclass];
    n->next = chunk;

    pagemap_set(n->m, n);

    heap->small[sizeclass] = n;
    chunk = n;
  }

  // get the first available slot and clear it
  uint32_t bit = __builtin_ffs(chunk->slots) - 1;
  chunk->slots &= ~(1 << bit);
  void* m = chunk->m + (bit << HEAP_MINBITS);

  // if we're full, move us to the full list
  if(chunk->slots == 0)
  {
    heap->small[sizeclass] = chunk->next;
    chunk->next = heap->small_full[sizeclass];
    heap->small_full[sizeclass] = chunk;
  }

  return m;
}

static void* large_malloc(actor_t* actor, heap_t* heap, size_t size)
{
  chunk_t* chunk = POOL_ALLOC(chunk_t);
  chunk->actor = actor;
  chunk->large = true;
  chunk->large_mark = false;

  chunk->len = size & HEAP_PAGEMASK;
  if((size & HEAP_PAGEMASK) != 0) { chunk->len += HEAP_PAGESIZE; }

  chunk->m = mmap(
    0,
    chunk->len,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANON,
    -1,
    0
    );

  if(chunk->m == MAP_FAILED) { abort(); }

  chunk->next = heap->large;
  heap->large = chunk;

  return chunk;
}

void heap_init(heap_t* heap)
{
  memset(heap, 0, sizeof(heap_t));
  heap->next_gc = HEAP_INITIALGC;
}

void heap_destroy(heap_t* heap)
{
  chunk_list(destroy_large, heap->large);

  for(int i = 0; i < HEAP_SIZECLASSES; i++)
  {
    chunk_list(destroy_small, heap->small[i]);
    chunk_list(destroy_small, heap->small_full[i]);
  }
}

void* heap_malloc(actor_t* actor, heap_t* heap, size_t size)
{
  if(size == 0)
  {
    return NULL;
  } else if(size <= sizeof(block_t)) {
    return small_malloc(actor, heap, size);
  } else {
    return large_malloc(actor, heap, size);
  }
}

void* heap_calloc(actor_t* actor, heap_t* heap, size_t count, size_t size)
{
  size_t total = count * size;

  // if the top half of the bits aren't set, we can't overflow
  // if they are set, use a divide to test for an overflow
  // divide is only triggered when asking for 4GB or more
  if(((count | size) & (SIZE_MAX << (sizeof(size_t) << 2))) &&
     ((total / size) != count)
    )
  {
    return NULL;
  }

  // doesn't memset to 0
  return heap_malloc(actor, heap, total);
}

bool heap_startgc(heap_t* heap)
{
  if(heap->used < heap->next_gc) { return false; }

  for(int i = 0; i < HEAP_SIZECLASSES; i++)
  {
    chunk_list(clear_small, heap->small[i]);
    chunk_list(clear_small, heap->small_full[i]);
  }

  chunk_list(clear_large, heap->large);
  return true;
}

bool heap_mark(chunk_t* chunk, void* p)
{
  // FIX: false sharing: reading from something that will never be written
  // but is on a cache line that will often be written
  bool marked;

  if(chunk->large)
  {
    marked = chunk->large_mark;
    chunk->large_mark = true;
  } else {
    // shift to account for smallest allocation size
    // p is always exactly aligned with a slot, it is never an internal pointer
    uint32_t slot = 1 << ((uintptr_t)(p - chunk->m) >> HEAP_MINBITS);

    // check if it was already marked
    marked = (chunk->slots & slot) == 0;

    // a clear bit is in-use, a set bit is available
    chunk->slots &= ~slot;
  }

  return marked;
}

void heap_endgc(heap_t* heap)
{
  size_t used = 0;

  for(int i = 0; i < HEAP_SIZECLASSES; i++)
  {
    chunk_t* list1 = heap->small[i];
    chunk_t* list2 = heap->small_full[i];

    heap->small[i] = NULL;
    heap->small_full[i] = NULL;

    chunk_t** avail = &heap->small[i];
    chunk_t** full = &heap->small_full[i];

    used += sweep_small(list1, avail, full, sizeclass_empty[i]);
    used += sweep_small(list2, avail, full, sizeclass_empty[i]);
  }

  heap->large = sweep_large(heap->large, &used);
  heap->used = used;
  heap->next_gc = used << 1;

  if(heap->next_gc < HEAP_INITIALGC)
  {
    heap->next_gc = HEAP_INITIALGC;
  }
}

actor_t* heap_owner(chunk_t* chunk)
{
  return chunk->actor;
}
