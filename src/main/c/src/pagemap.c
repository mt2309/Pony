#include "pagemap.h"
#include "heap.h"
#include <stddef.h>
#include <stdint.h>
#include <sys/mman.h>

#define PAGEMAP_ADDRESSBITS 48
#define PAGEMAP_LEVELS 3
#define PAGEMAP_BITS (PAGEMAP_ADDRESSBITS - HEAP_MAXBITS) / PAGEMAP_LEVELS
#define PAGEMAP_EXCESS (PAGEMAP_ADDRESSBITS - HEAP_MAXBITS) % PAGEMAP_LEVELS

#define L1_MASK (PAGEMAP_BITS)
#define L2_MASK (PAGEMAP_BITS + (PAGEMAP_EXCESS > 1))
#define L3_MASK (PAGEMAP_BITS + (PAGEMAP_EXCESS > 0))

#define L1_SHIFT (HEAP_MAXBITS + L1_MASK)
#define L2_SHIFT (L1_SHIFT + L2_MASK)
#define L3_SHIFT (L2_SHIFT + L3_MASK)

typedef struct pagemap_level_t
{
  int shift;
  int mask;
  size_t size;
} pagemap_level_t;

/* Constructed for a 48 bit address space where we are interested in memory
 * with 11 bit (2048 byte) granularity.
 *
 * At the first level, we shift 35 and mask on the low 13 bits, giving us bits
 * 35-47. At the second level, we shift 23 and mask on the low 12 bits, giving
 * us bits 23-34. At the third level, we shift 11 and mask on the low 12 bits,
 * giving us bits 11-22. Bits 0-10 and 48-63 are always ignored.
 */
static const pagemap_level_t level[PAGEMAP_LEVELS] =
{
  { L3_SHIFT, (1 << L3_MASK) - 1, (1 << L3_MASK) * sizeof(void*) },
  { L2_SHIFT, (1 << L2_MASK) - 1, (1 << L2_MASK) * sizeof(void*) },
  { L1_SHIFT, (1 << L1_MASK) - 1, (1 << L1_MASK) * sizeof(void*) }
};

static void** root = NULL;

void* pagemap_get(const void* m)
{
  void** v = root;

  for(int i = 0; i < PAGEMAP_LEVELS; i++)
  {
    if(v == NULL)
    {
      return NULL;
    }

    uintptr_t ix = ((uintptr_t)m >> level[i].shift) & level[i].mask;
    v = v[ix];
  }

  return v;
}

void pagemap_set(const void* m, void* v)
{
  void*** pv = &root;

  for(int i = 0; i < PAGEMAP_LEVELS; i++)
  {
    if(*pv == NULL)
    {
      void** p = mmap(
        0,
        level[i].size,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANON,
        -1,
        0
        );

      if(!__sync_bool_compare_and_swap(pv, NULL, p))
      {
        munmap(p, level[i].size);
      }
    }

    uintptr_t ix = ((uintptr_t)m >> level[i].shift) & level[i].mask;
    pv = (void***)&((*pv)[ix]);
  }

  *pv = v;
}
