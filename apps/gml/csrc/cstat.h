// Functions used to accumulate statistics about the program being
// simulated.
//
// These are written in C because 64-bit overflow is likely!

#include "core.h"

#define INIT_TIMER(X) unsigned int asdf_##X##_hi, asdf_##X##_lo;

#define START_TIMER(X) rdtsc asdf_##X##_hi : asdf_##X##_lo;

#define END_TIMER(X) { unsigned int hi,lo; \
rdtsc hi : lo; \
stat_timer(X##_timer,asdf_##X##_hi,asdf_##X##_lo,hi,lo); }

extern timer;

extern timer syscall_timer;

extern void stat_timer(timer t,
		      unsigned int start_hi, unsigned int start_lo, 
		      unsigned int end_hi, unsigned int end_lo);

extern void rtcg_stat_output(FILE f, Cstring flags, Cstring prog_name);
