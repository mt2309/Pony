#ifndef set_h
#define set_h

#include <stdbool.h>

typedef enum
{
  SET_NEW,
  SET_MARKED,
  SET_PRESENT,
} set_ret_t;

typedef struct set_t set_t;

set_t* set_alloc();

void set_free(set_t* set);

set_ret_t set_add(set_t* set, void* p, bool marked);

bool set_remove(set_t* set, void* p);

void set_clear(set_t* set);

set_t* set_sweep(set_t* set);

set_t* set_clone(set_t* set);

#endif
