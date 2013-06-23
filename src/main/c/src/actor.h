#ifndef actor_h
#define actor_h

#include <pony/pony.h>
#include <stdbool.h>

bool actor_run(actor_t* actor);

void actor_destroy(actor_t* actor);

#endif
