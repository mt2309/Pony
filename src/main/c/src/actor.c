#include "actor.h"
#include "actorq.h"
#include "heap.h"
#include "pool.h"
#include "set.h"
#include "pagemap.h"
#include "scheduler.h"
#include <assert.h>

struct actor_t
{
  dispatch_t disp;
  type_fn final;

  void* p;
  type_t* type;

  bool blocked;
  uint32_t rc;

  heap_t heap;
  actorq_t q;

  set_t* ext_actors;
  set_t* ext_objects;
};

POOL_CREATE(actor_t);
POOL_REFERENCE(message_t);

static __thread actor_t* this;

static void push_message(actor_t* to, uint64_t id, arg_t arg, type_t* type)
{
  message_t* msg = POOL_ALLOC(message_t);
  msg->id = id;
  msg->arg = arg;
  msg->type = type;

  if(actorq_push(&to->q, msg))
  {
    scheduler_add(to);
  }
}

static bool handle_message(message_t* msg)
{
  switch(msg->id)
  {
    // FIX: RC messages, return false

    /* FIX: cycle collect message. tells actor to zero its rc and to remove the
    actors in the cycle list (and all objects owned by those actors) from
    the external set.

    the problem is finalisation: these actors reference each other, but must
    not send messages to each other, because they may not exist. but must send
    gc messages to other actors.
    */

    default:
      if(this->blocked)
      {
        // FIX: cycle_unblock(this);
        this->blocked = false;
      }

      if(msg->type != NULL)
      {
        // FIX: type walk, insert into external set, handle RC
      }

      this->disp(this, this->p, this->type, msg->id, msg->arg);
      return true;
  }
}

static void destroy_self()
{
  if(this->final != NULL)
  {
    this->final(this->p);
  }

  // FIX: sweep the externset to generate gc messages to other actors

  while(actorq_pop(&this->q) != NULL);
  actorq_destroy(&this->q);

  // FIX: foreign and external set destroy
  heap_destroy(&this->heap);

  POOL_FREE(actor_t, this);
}

bool actor_run(actor_t* actor)
{
  this = actor;
  message_t* msg;

  while((msg = actorq_pop(&this->q)) != NULL)
  {
    if(handle_message(msg)) { return true; }
  }

  if(this->rc == 0)
  {
    assert(!this->blocked);
    destroy_self();
    return false;
  }

  if(!this->blocked)
  {
    // FIX: cycle_block(this);
    this->blocked = true;
  }

  if(heap_startgc(&this->heap))
  {
    if(this->type != NULL)
    {
      switch(this->type->mode)
      {
        case PONY_ACTOR:
          pony_markactor(this->p);
          break;

        case PONY_ISOLATED:
          pony_markisolated(this->p, this->type->mark);
          break;

        case PONY_WRITEABLE:
          pony_markwriteable(this->p, this->type->mark);
          break;

        case PONY_FROZEN:
          pony_markfrozen(this->p, this->type->mark);
          break;

        case PONY_OPAQUE:
          pony_markopaque(this->p);
          break;
      }
    }

    heap_endgc(&this->heap);
    // FIX: sweep external set
  }

  return !actorq_markempty(&this->q);
}

actor_t* pony_create(dispatch_t disp, type_fn final)
{
  actor_t* actor = POOL_ALLOC(actor_t);
  actor->disp = disp;
  actor->final = final;

  actor->p = NULL;
  actor->type = NULL;

  actor->blocked = true;
  actor->rc = 1;

  actorq_init(&actor->q);
  heap_init(&actor->heap);

  if(this != NULL)
  {
    // FIX: add to out external set
  }

  return actor;
}

void pony_set(void* p, type_t* type)
{
  this->p = p;
  this->type = type;
}

actor_t* pony_send(actor_t* to, uint64_t id, void* p, type_t* type)
{
  if(type != NULL)
  {
    // FIX: type walk to do distributed RC
  }

  arg_t arg = {.p = p};
  push_message(to, id, arg, type);

  return to;
}

void pony_sendi(actor_t* to, uint64_t id, intptr_t i)
{
  arg_t arg = {.i = i};
  push_message(to, id, arg, NULL);
}

void pony_sendd(actor_t* to, uint64_t id, double d)
{
  arg_t arg = {.d = d};
  push_message(to, id, arg, NULL);
}

void pony_markactor(actor_t* actor)
{
  if(this->ext_actors == NULL) this->ext_actors = set_alloc();

  if(set_add(this->ext_actors, actor, true) == SET_NEW)
  {
    // FIX: rcinc(chunk);
  }
}

void pony_markisolated(void* p, type_fn f)
{
  chunk_t* chunk = pagemap_get(p);
  bool previously_marked;

  if(heap_owner(chunk) == this)
  {
    previously_marked = heap_mark(chunk, p);
  } else {
    if(this->ext_objects == NULL) this->ext_objects = set_alloc();

    switch(set_add(this->ext_objects, p, true))
    {
      case SET_NEW:
        // FIX: rcinc(p)
        previously_marked = false;
        break;

      case SET_MARKED:
        previously_marked = false;
        break;

      default:
        previously_marked = true;
    }
  }

  if(!previously_marked && f) f(p);
}

void pony_markwriteable(void* p, type_fn f)
{
  pony_markisolated(p, f);
}

void pony_markfrozen(void* p, type_fn f)
{
  chunk_t* chunk = pagemap_get(p);

  if(heap_owner(chunk) == this)
  {
    if(!heap_mark(chunk, p) && f) f(p);
  } else {
    if(this->ext_objects == NULL) this->ext_objects = set_alloc();

    if(set_add(this->ext_objects, p, true) == SET_NEW)
    {
      // FIX: rcinc(p)
    }
  }
}

void pony_markopaque(void* p)
{
  chunk_t* chunk = pagemap_get(p);

  if(heap_owner(chunk) == this)
  {
    heap_mark(chunk, p);
  } else {
    if(this->ext_objects == NULL) this->ext_objects = set_alloc();

    if(set_add(this->ext_objects, p, true) == SET_NEW)
    {
      // FIX: rcinc(p)
    }
  }
}
