#if defined(__linux__)
#define _GNU_SOURCE
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#elif defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach.h>
#include <mach/thread_policy.h>
#endif

#include "cpu.h"

uint32_t cpu_count()
{
#if defined(__linux__)
  return sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(__APPLE__)
  int count;
  size_t len = sizeof(int);
  sysctlbyname("hw.physicalcpu", &count, &len, NULL, 0);
  return count;
#endif
}

bool cpu_hyperthread(uint32_t cpu)
{
#if defined(__linux__)
  char file[FILENAME_MAX];
  snprintf(file, FILENAME_MAX,
    "/sys/devices/system/cpu/cpu%d/topology/thread_siblings_list", cpu);

  FILE* fp = fopen(file, "r");

  if(fp != NULL)
  {
    const int namelen = 16;
    char name[namelen];

    size_t len = fread(name, 1, namelen, fp);
    name[len] = '\0';
    fclose( fp );

    if(cpu != atoi(name)) { return true; }
  }
#endif

  return false;
}

void cpu_affinity(uint32_t cpu)
{
#if defined(__linux__)
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(cpu, &set);
  sched_setaffinity(0, 1, &set);
#elif defined(__APPLE__)
  thread_affinity_policy_data_t policy;
  policy.affinity_tag = cpu;
  thread_policy_set(mach_thread_self(), THREAD_AFFINITY_POLICY,
    (thread_policy_t)&policy, THREAD_AFFINITY_POLICY_COUNT);
#endif
}
