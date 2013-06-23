#include "scheduler.h"
#include "cpu.h"
#include "mpmcq.h"
#include <pony/pony.h>
#include <pthread.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  pthread_t tid;
  uint32_t cpu;
  uint32_t steal;
  uint64_t last;
  bool tick;
  mpmcq_t q;
} schedthr_t;

static const struct timespec ts_zero;
static uint32_t schedthr_count;
static schedthr_t* schedthr;
static volatile bool running;
static volatile int exit_code;
static __thread schedthr_t* schedthr_this;

static uint64_t rdtsc()
{
  uint32_t low, high;

  asm volatile
  (
    "RDTSC\n"
    "mov %%edx, %0\n"
    "mov %%eax, %1\n"
    : "=r" (high), "=r" (low) :: "%rax", "%rdx"
  );

  return ((uint64_t)high << 32) | low;
}

static void pause()
{
  if(schedthr_this->tick)
  {
    uint64_t now = rdtsc();
    uint64_t e = now - schedthr_this->last;

    if(e > 100 * 1000 * 1000)
    {
      schedthr_this->last = now;
      // FIX: cycle_tick();
      return;
    }
  }

  nanosleep(&ts_zero, NULL);
}

static actor_t* next_actor(actor_t* prev)
{
  actor_t* next = mpmcq_pop(&schedthr_this->q);

  if((next == NULL) && (schedthr_count > 1))
  {
    schedthr_t* from;

    do
    {
      schedthr_this->steal = rand_r(&schedthr_this->steal);
      from = &schedthr[schedthr_this->steal % schedthr_count];
    } while(from == schedthr_this);

    next = mpmcq_pop(&from->q);
  }

  if(next != NULL)
  {
    if(prev != NULL)
    {
      mpmcq_push(&schedthr_this->q, prev);
    }

    return next;
  }

  return prev;
}

static void* run_thread(void* arg)
{
  schedthr_this = arg;
  cpu_affinity(schedthr_this->cpu);

  actor_t* actor = NULL;

  while(true)
  {
    if((actor = next_actor(actor)) != NULL)
    {
      if(!actor_run(actor))
      {
        actor = NULL;
      }
    } else {
      if(running)
      {
        pause();
      } else {
        break;
      }
    }
  }

  return NULL;
}

static int parse_args(int argc, char** argv, uint32_t* threads)
{
  const char* tag = "--pony";
  const size_t len = strlen(tag);

  for(int i = 0; i < argc; i++)
  {
    if(!strncmp(argv[i], tag, len))
    {
      int remove = 0;

      if(!strcmp(argv[i] + len, "threads"))
      {
        remove++;

        if(i < (argc - 1))
        {
          remove++;
          *threads = atoi(argv[i + 1]);
        }
      }

      argc -= remove;
      memmove(&argv[i], &argv[i + remove], argc - i);
    }
  }

  return argc;
}

void scheduler_add(actor_t* actor)
{
  mpmcq_push(&schedthr_this->q, actor);
}

void scheduler_stop()
{
  running = false;
}

int pony_start(int argc, char** argv, actor_t* actor)
{
  uint32_t threads = 0;
  argc = parse_args(argc, argv, &threads);

  uint32_t count = cpu_count();
  if((threads == 0) || (threads > count)) { threads = count; }
  schedthr_count = 0;

  for(uint32_t i = 0; (i < count) && (schedthr_count < threads); i++)
  {
    if(!cpu_hyperthread(i)) { schedthr_count++; }
  }

  schedthr_t schedthr_stack[schedthr_count];
  schedthr = schedthr_stack;
  memset(schedthr, 0, schedthr_count * sizeof(schedthr_t));
  threads = 0;

  for(uint32_t i = 0; (i < count) && (threads < schedthr_count); i++)
  {
    if(!cpu_hyperthread(i))
    {
      schedthr[threads].cpu = i;
      mpmcq_init(&schedthr[threads].q);
      threads++;
    }
  }

  schedthr[0].tick = true;
  schedthr[0].tid = pthread_self();
  schedthr_this = &schedthr[0];
  running = true;

  for(uint32_t i = 1; i < schedthr_count; i++)
  {
    if(pthread_create(&schedthr[i].tid, NULL, run_thread, &schedthr[i]) != 0)
    {
      running = false;
      return -1;
    }
  }

  pony_main_t arg = {argc, argv};
  pony_send(actor, PONY_MAIN, &arg, NULL);

  run_thread(&schedthr[0]);

  for(uint32_t i = 1; i < schedthr_count; i++)
  {
    pthread_join(schedthr[i].tid, NULL);
  }

  for(uint32_t i = 0; i < schedthr_count; i++)
  {
    mpmcq_destroy(&schedthr[i].q);
  }

  return exit_code;
}

void pony_stop()
{
  // FIX: remove this
  scheduler_stop();
}

void pony_exitcode(int code)
{
  exit_code = code;
}
