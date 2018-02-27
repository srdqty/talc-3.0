#include <stdio.h>
#include <stdlib.h>

extern void qsort(void *base, size_t nmemb, size_t size,
		  int (*compar)(const void *, const void *));

#include "cc.h"
extern unsigned long long rdtsr();

#define MAX_TIMES 10000

unsigned long long times[MAX_TIMES];

int compare_long_long(void *a, void *b)
{
  unsigned long long al = *(unsigned long long *)a;
  unsigned long long bl = *(unsigned long long *)b;

  if (al < bl)
    return -1;
  else if (al > bl)
    return 1;
  else
    return 0;
}

unsigned long long calc_median(int i)
{
  qsort(times,i,sizeof(unsigned long long),compare_long_long);
  return times[i/2];
}

void start_cc_time(int i)
{
  times[i] = rdtsr();
}

void end_cc_time(int i)
{
  times[i] = rdtsr() - times[i];
}

void print_cc_time(int index, int iters)
{
  unsigned long long scaled_cycs = times[index] / (unsigned long long)iters;
  printf("time %d: %Lu cyc, %Lu usec\n", 
	 index, scaled_cycs,
	 scaled_cycs / (unsigned long long)MACHINE_CLOCKRATE_MHZ);
}

void print_cc_time_med(int num, int iters_per_num)
{
  unsigned long long tot_cyc =
    calc_median(num) / (unsigned long long)iters_per_num;
  printf("median time %Lu cyc, %Lu usec\n", 
	 tot_cyc, tot_cyc / (unsigned long long)MACHINE_CLOCKRATE_MHZ);
}
