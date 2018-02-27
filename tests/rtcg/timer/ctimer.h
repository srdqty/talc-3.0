
#ifndef TIMER_H
#define TIMER_H

void register_program(char *name, char compiler, char kind, char *params);

void log_timer(char c, unsigned int start_hi, unsigned int start_lo,
	       unsigned int end_hi, unsigned int end_lo);

void output_timers(char *filename);

#endif
