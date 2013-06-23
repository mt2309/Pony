#include "set.h"
#include "hash.h"
#include "pool.h"
#include <string.h>

#define SET_NODES (1 << 6)
#define SET_MASK (SET_NODES - 1)
#define SET_SIZE 7

typedef struct chain_t
{
  void* p[SET_SIZE];
  struct chain_t* next;
} chain_t;

struct set_t
{
  chain_t* nodes[SET_NODES];
};

POOL_CREATE(chain_t);
POOL_CREATE(set_t);

set_t* set_alloc()
{
  set_t* set = POOL_ALLOC(set_t);
  memset(set, 0, sizeof(set_t));
  return set;
}

void set_free(set_t* set)
{
  set_clear(set);
  POOL_FREE(set_t, set);
}

set_ret_t set_add(set_t* set, void* p, bool marked)
{
  int index = hash_address(p) & SET_MASK;
  void* mark = (void*)((uintptr_t)p | 1);
  chain_t* node = set->nodes[index];

  chain_t* open = NULL;
  int open_index = 0;

  while(node != NULL)
  {
    for(int i = 0; i < SET_SIZE; i++)
    {
      if(node->p[i] == p)
      {
        if(marked)
        {
          node->p[i] = mark;
          return SET_MARKED;
        } else {
          return SET_PRESENT;
        }
      } else if(node->p[i] == mark) {
        return SET_PRESENT;
      } else if((open == NULL) && (node->p[i] == NULL)) {
        open = node;
        open_index = i;
      }
    }

    node = node->next;
  }

  if(open != NULL)
  {
    open->p[open_index] = marked ? mark : p;
  } else {
    node = POOL_ALLOC(chain_t);
    memset(node, 0, sizeof(chain_t));
    node->p[0] = marked ? mark : p;
    node->next = set->nodes[index];
    set->nodes[index] = node;
  }

  return SET_NEW;
}

bool set_remove(set_t* set, void* p)
{
  int index = hash_address(p) & SET_MASK;
  chain_t* node = set->nodes[index];

  while(node != NULL)
  {
    for(int i = 0; i < SET_SIZE; i++)
    {
      if(node->p[i] == p)
      {
        node->p[i] = NULL;
        return true;
      }
    }

    node = node->next;
  }

  return false;
}

void set_clear(set_t* set)
{
  for(int i = 0; i < SET_NODES; i++)
  {
    if(set->nodes[i] != NULL)
    {
      chain_t* node = set->nodes[i];
      chain_t* next;

      do
      {
        next = node->next;
        POOL_FREE(chain_t, node);
        node = next;
      } while(node != NULL);

      set->nodes[i] = NULL;
    }
  }
}

set_t* set_sweep(set_t* set)
{
  set_t* clone = set_alloc();

  for(int i = 0; i < SET_NODES; i++)
  {
    if(set->nodes[i] != NULL)
    {
      chain_t* this = set->nodes[i];
      chain_t* head = NULL;
      chain_t* that = NULL;
      chain_t* next;
      int that_index = SET_SIZE;

      do
      {
        for(int i = 0; i < SET_SIZE; i++)
        {
          if((uintptr_t)this->p[i] & 1)
          {
            this->p[i] = (void*)((uintptr_t)this->p[i] & ~(uintptr_t)1);
          } else {
            if(that_index == SET_SIZE)
            {
              next = POOL_ALLOC(chain_t);

              if(head == NULL)
              {
                head = next;
              } else {
                that->next = next;
              }

              that = next;
              that->p[0] = this->p[i];
              that_index = 1;
            } else {
              that->p[that_index++] = this->p[i];
            }

            this->p[i] = NULL;
          }
        }

        this = this->next;
      } while(this != NULL);

      clone->nodes[i] = head;
    }
  }

  return clone;
}

set_t* set_clone(set_t* set)
{
  set_t* clone = set_alloc();

  for(int i = 0; i < SET_NODES; i++)
  {
    if(set->nodes[i] != NULL)
    {
      chain_t* this = set->nodes[i];
      chain_t* that = POOL_ALLOC(chain_t);
      clone->nodes[i] = that;
      memcpy(that, this, sizeof(chain_t));

      while(this->next != NULL)
      {
        that->next = POOL_ALLOC(chain_t);
        this = this->next;
        that = that->next;
        memcpy(that, this, sizeof(chain_t));
      }
    }
  }

  return clone;
}
