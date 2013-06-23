#ifndef actorq_h
#define actorq_h

#include <pony/pony.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct message_t
{
  uint64_t id;
  arg_t arg;
  type_t* type;
  volatile struct message_t* next;
} message_t;

typedef struct actorq_t
{
  volatile message_t* head;
  message_t* tail;
} actorq_t;

void actorq_init(actorq_t* q);

void actorq_destroy(actorq_t* q);

bool actorq_push(actorq_t* q, message_t* msg);

message_t* actorq_pop(actorq_t* q);

bool actorq_markempty(actorq_t* q);

#endif
