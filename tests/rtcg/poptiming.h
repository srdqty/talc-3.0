
#ifndef POP_TIMING_H
#define POP_TIMING_H

#include "core.h"
#include "timer/timer.h"

#define INIT_TIMER(X) unsigned int asdf_##X##_hi, asdf_##X##_lo;

#define START_TIMER(X) rdtsc asdf_##X##_hi : asdf_##X##_lo;

#define END_TIMER(X) { unsigned int hi,lo; \
rdtsc hi : lo; \
log_timer(X,asdf_##X##_hi,asdf_##X##_lo,hi,lo); }

#define OUTPUT_TIMERS_INTERNAL(L) output_timers(string_to_Cstring(#L));

#define C_IS_CRAZY(L) OUTPUT_TIMERS_INTERNAL(L)
#define OUTPUT_TIMERS C_IS_CRAZY(LOGFILE)

#define POP_COMPILER 'i'
#define SB_COMPILER 'o'
#define C_COMPILER 'c'
#define RTCG_KIND 'r'
#define PLAIN_KIND 'c'
#define POPC_KIND 'p'

#define REGISTER(NAME,COMPILER,KIND,ARGS) register_program(string_to_Cstring(NAME),COMPILER,KIND,string_to_Cstring(ARGS))

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
  REGISTER(NAME,COMPILER,KIND,ARGS); \
  INIT_TIMER(1); \
  INIT_TIMER(2); \
  INIT_TIMER(3); \
  INIT_TIMER(4); \
  for(int i=0; i<10; i++) { \
    START_TIMER(1); \
    GEN_STMT; \
    END_TIMER(1); \
  } \
  GEN_STMT; \
  for(int i=0; i<10; i++) { \
    START_TIMER(2); \
    RUN_STMT; \
    END_TIMER(2); \
  } \
  for(int j = 0; j<10; j++) { \
    START_TIMER(3); \
    for(int i=0; i<10;i++) { \
      RUN_STMT; \
    } \
    END_TIMER(3); \
    START_TIMER(4); \
    for(int i=0; i<100;i++) { \
      RUN_STMT; \
    } \
    END_TIMER(4); \
  } \
  output_timers(string_to_Cstring("perf.log"));
#endif
