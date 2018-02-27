#ifndef __PROF_H
#define __PROF_H

#ifdef __linux__
typedef long long int int64;
#else
typedef __int64 int64;
#endif

/* Keep in sync with profiler.ml. */
typedef struct {
  int64 cycles; /* How many cycles did this function use. */
  int64 child_cycles; /* Counts of child cycles. */
  int64 within_count; /* # of calls occuring within outermost calls to this function. */
  int64 outermost_count;
  int64 outermost_cycles; /* Time spent in outermost calls. */
  int   depth; /* Number of active recursive invocations of this function */
  char *name;
} prof_s, *prof_t;

extern prof_t prof_data[];
extern int prof_data_size;

void prof_init(); /* Initialize the profiler. */
void prof_enter(prof_t); /* Call on function entry. */
void prof_leave(prof_t); /* Call on function exit. */
void prof_handler(prof_t); /* Call on handler entry. */
void prof_end(); /* Generate a summary of the collected statistics. */

#endif
