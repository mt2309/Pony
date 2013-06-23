#include "mpmcq.h"
#include "pool.h"

struct mpmcq_node_t
{
  volatile struct mpmcq_node_t* next;
  void* data;
};

POOL_CREATE(mpmcq_node_t);

void mpmcq_init( mpmcq_t* q )
{
  mpmcq_node_t* stub = mpmcq_alloc();
  q->head = stub;
  q->tail.node = stub;
}

void mpmcq_destroy(mpmcq_t* q)
{
  POOL_FREE(mpmcq_node_t, q->tail.node);
  q->head = NULL;
  q->tail.node = NULL;
}

mpmcq_node_t* mpmcq_alloc()
{
  mpmcq_node_t* node = POOL_ALLOC(mpmcq_node_t);
  node->data = NULL;
  node->next = NULL;

  return node;
}

void mpmcq_free(mpmcq_node_t* node)
{
  POOL_FREE(mpmcq_node_t, node);
}

void mpmcq_push(mpmcq_t* q, void* data)
{
  mpmcq_pushnode(q, mpmcq_alloc(), data);
}

void mpmcq_pushnode(mpmcq_t* q, mpmcq_node_t* node, void* data)
{
  node->data = data;
  mpmcq_node_t* prev = (mpmcq_node_t*)__sync_lock_test_and_set(&q->head, node);
  prev->next = node;
}

void* mpmcq_pop(mpmcq_t* q)
{
  mpmcq_dwcas_t cmp, xchg;
  mpmcq_node_t* next;
  void* data;

  do
  {
    cmp = q->tail;
    next = (mpmcq_node_t*)cmp.node->next;

    if(next == NULL)
    {
      return NULL;
    }

    data = next->data;
    xchg.node = next;
    xchg.aba = cmp.aba + 1;
  } while(!__sync_bool_compare_and_swap(&q->tail.dw, cmp.dw, xchg.dw));

  mpmcq_free(cmp.node);
  return data;
}
