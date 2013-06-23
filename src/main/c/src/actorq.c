#include "actorq.h"
#include "pool.h"
#include <assert.h>

POOL_CREATE(message_t);

void actorq_init(actorq_t* q)
{
  message_t* stub = POOL_ALLOC(message_t);
  q->head = (message_t*)((uintptr_t)stub | 1);
  q->tail = stub;
}

void actorq_destroy(actorq_t* q)
{
  assert(((uintptr_t)q->head & ~(uintptr_t)1) == (uintptr_t)q->tail);

  POOL_FREE(message_t, q->tail);
  q->head = NULL;
  q->tail = NULL;
}

bool actorq_push(actorq_t* q, message_t* msg)
{
  msg->next = NULL;
  message_t* prev = (message_t*)__sync_lock_test_and_set(&q->head, msg);

  bool was_empty = ((uintptr_t)prev & 1) != 0;
  prev = (message_t*)((uintptr_t)prev & ~(uintptr_t)1);
  prev->next = msg;

  return was_empty;
}

message_t* actorq_pop(actorq_t* q)
{
  message_t* tail = q->tail;
  message_t* next = (message_t*)tail->next;

  if(next != NULL)
  {
    q->tail = next;
    POOL_FREE(message_t, tail);
  }

  return next;
}

bool actorq_markempty(actorq_t* q)
{
  message_t* tail = q->tail;
  message_t* head = (message_t*)q->head;

  if(((uintptr_t)head & 1) != 0) { return true; }
  if(head != tail) { return false; }

  head = (message_t*)((uintptr_t)head | 1);
  return __sync_bool_compare_and_swap(&q->head, tail, head);
}
