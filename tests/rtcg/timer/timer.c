/* C file that manages Popcorn timer information. */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NUM_TIMERS 32

#ifdef __linux__
typedef long long int64;
typedef unsigned long long uint64;

#define PLD  "lld"
#else
typedef __int64 int64;
typedef unsigned __int64 uint64;

#define PLD "I64d"
#endif

typedef struct {
  int64 min; /* Least logged value. */
  int64 max; /* Least logged value. */
  int64 cum; /* Cumulative clock cycles. */
  int64 num; /* Number of times counter was logged. */
} timer_s;

typedef struct {
  char *name;
  char kind;
  char compiler;
  char *params;
} prog_info;

prog_info p_info = {"",0,0,""};

timer_s timers[NUM_TIMERS] = {0}; // Everything starts at 0;

void register_program(char *name, char compiler, char kind, char *params) {
  if(name!=NULL) p_info.name = name;
  p_info.compiler = compiler;
  p_info.kind = kind;
  if(params!=NULL) p_info.params = params;
}

void log_timer(char i, unsigned int start_hi, unsigned int start_lo,
	       unsigned int   end_hi, unsigned int   end_lo) {

  uint64 start = (uint64)((((uint64)start_hi) << 32) | start_lo);
  uint64   end = (uint64)((((uint64)  end_hi) << 32) |   end_lo);

  int64  diff = end - start;

  timer_s t = timers[i];

  if(t.num == 0) {
    t.min = diff;
    t.max = diff;
  } else {
    if(t.min>diff) t.min = diff;
    if(t.max<diff) t.max = diff;
  }

  t.cum += diff;
  t.num++;

  timers[i] = t;

  return;
}

// Append all timers that have logged events to the specified file.
void output_timers(char *filename) {
  FILE *fp = fopen(filename,"a");
  int i = 0;
  time_t t;

  if(fp==NULL) {
    fprintf(stderr,"Open failed for file %s.\n", filename);
    return;
  }

  //t = time(NULL);
  //fprintf(fp,"%% %s",ctime(&t)); //Apparently ctime \n terminates the string?
  for(i=0;i<NUM_TIMERS;i++) {
    if(timers[i].num != 0) {
      timer_s t = timers[i];
     
      fprintf(fp,"%-10s %1c %1c %3d %12"PLD" %12"PLD" %12"PLD" %5"PLD" %s \n",
	      p_info.name, p_info.compiler, p_info.kind, 
	      i, t.min, t.max, t.cum, t.num,
	      p_info.params);
    }
  }

  fclose(fp);
}


