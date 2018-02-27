
#ifndef C_TIMING_H
#define C_TIMING_H

#include "timer/ctimer.h"

#define INIT_TIMER(X) unsigned int __asdf_##X##_hi, __asdf_##X##_lo

#ifdef __linux__

#define START_TIMER(X) __asm__ __volatile__("\tpushal\n"            \
					    "\trdtsc\n"             \
					    "\tmovl %%edx,%0\n"     \
					    "\tmovl %%eax,%1\n"     \
					    "\tpopal\n" :           \
					    "=m" (__asdf_##X##_hi), \
					    "=m" (__asdf_##X##_lo))

#define END_TIMER(X) { unsigned int hi,lo;                      \
                       __asm__ __volatile__("\tpushal\n"        \
					    "\trdtsc\n"         \
					    "\tmovl %%edx,%0\n" \
					    "\tmovl %%eax,%1\n" \
					    "\tpopal\n" :       \
					    "=m" (hi), "=m" (lo));  \
		       log_timer(X,__asdf_##X##_hi,__asdf_##X##_lo,hi,lo); }

#else

#define RDTSC __asm _emit 0fh __asm _emit 031h

#define START_TIMER(X) __asm      \
{                                 \
   __asm pushad                   \
   RDTSC                          \
   __asm mov __asdf_##X##_hi, edx \
   __asm mov __asdf_##X##_lo, eax \
   __asm popad                    \
}

#define END_TIMER(X) { unsigned int hi,lo; \
__asm {                                    \
  __asm pushad                             \
  RDTSC                                    \
  __asm mov hi, edx                        \
  __asm mov lo, eax                        \
  __asm popad                              \
}                                          \
log_timer(X,__asdf_##X##_hi,__asdf_##X##_lo,hi,lo); }

#endif

#define OUTPUT_TIMERS_INTERNAL(L) output_timers(#L)

#define C_IS_CRAZY(L) OUTPUT_TIMERS_INTERNAL(L)
#define OUTPUT_TIMERS C_IS_CRAZY(LOGFILE)


#define POP_COMPILER 'i'
#define SB_COMPILER 'o'
#define C_COMPILER 'c'
#define RTCG_KIND 'r'
#define PLAIN_KIND 'c'
#define POPC_KIND 'p'

#define REGISTER(NAME,COMPILER,KIND,ARGS) register_program((NAME),COMPILER,KIND,(ARGS))

// Generic testing procedure
// NAME = name of benchmark
// COMPILER = character bound to compiler (POP, SB, C)
// KIND = character kind of file (RTCG, PLAIN or POPC)
// ARGS = string containing input arguments to main
// GEN_STMT = statement used to generate code (may be empty)
// RUN_STMT = statement used to run one iteration.
// We run:
//  1. The generator 10 times (takes out cache effects)
//  2. Time 10 individual runs (greater timer error)
//  3. Time 100 runs three times
// Output goes in NAME.log file.  NAME must be a literal string!
#define MAIN_TEST(NAME,COMPILER,KIND,ARGS,GEN_STMT,RUN_STMT) \
  { INIT_TIMER(1); \
  INIT_TIMER(2); \
  INIT_TIMER(3); \
  INIT_TIMER(4); \
  int i,j; \
  REGISTER(NAME,COMPILER,KIND,ARGS); \
  for(i=0; i<10; i++) { \
    START_TIMER(1); \
    GEN_STMT; \
    END_TIMER(1); \
  } \
  GEN_STMT; \
  for(i=0; i<10; i++) { \
    START_TIMER(2); \
    RUN_STMT; \
    END_TIMER(2); \
  } \
  for(j = 0; j<10; j++) { \
    START_TIMER(3); \
    for(i=0; i<10;i++) { \
      RUN_STMT; \
    } \
    END_TIMER(3); \
    START_TIMER(4); \
    for(i=0; i<100;i++) { \
      RUN_STMT; \
    } \
    END_TIMER(4); \
  } \
      output_timers("perf.log"); }

#endif
