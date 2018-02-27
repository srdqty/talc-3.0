
#ifndef TIMER_H
#define TIMER_H

#include "core.h"

// kind values are 0 (popcorn), 1 (rtcg), 2(C), 3(popc)
extern void register_program(Cstring name, char compiler, char kind, Cstring params);

extern void log_timer(char c, unsigned int start_hi, unsigned int start_lo,
		 unsigned int end_hi, unsigned int end_lo);

extern void output_timers(Cstring filename);

#endif
