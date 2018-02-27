// Functions used to accumulate statistics about the program being
// simulated.
//
// These are written in C because 64-bit overflow is likely!

#include "core.h"

/* instruction flags */
#define F_ICOMP		0x00000001	/* integer computation */
#define F_FCOMP		0x00000002	/* FP computation */
#define F_CTRL		0x00000004	/* control inst */
#define F_UNCOND	0x00000008	/*   unconditional change */
#define F_COND		0x00000010	/*   conditional change */
#define F_MEM		0x00000020	/* memory access inst */
#define F_LOAD		0x00000040	/*   load inst */
#define F_STORE		0x00000080	/*   store inst */
#define F_DISP		0x00000100	/*   displaced (R+C) addr mode */
#define F_RR		0x00000200	/*   R+R addr mode */
#define F_DIRECT	0x00000400	/*   direct addressing mode */
#define F_TRAP		0x00000800	/* traping inst */
#define F_LONGLAT	0x00001000	/* long latency inst (for sched) */
#define F_DIRJMP	0x00002000	/* direct jump */
#define F_INDIRJMP	0x00004000	/* indirect jump */
#define F_CALL		0x00008000	/* function call */
#define F_FPCOND	0x00010000	/* FP conditional branch */

#define INIT_TIMER(X) unsigned int asdf_##X##_hi, asdf_##X##_lo;

#define START_TIMER(X) rdtsc asdf_##X##_hi : asdf_##X##_lo;

#define END_TIMER(X) { unsigned int hi,lo; \
rdtsc hi : lo; \
stat_timer(X##_timer,asdf_##X##_hi,asdf_##X##_lo,hi,lo); }

// Pass in the instruction flags for each instruction.
extern void stat_op_tick(int msk, int iflags);

extern void stat_output(FILE f, Cstring prog_name);

extern void stat_segment_info(int text_size, int data_size, int stack_size);

extern void stat_alloc_pages(int num_pages);

extern timer;

extern timer syscall_timer;

extern void stat_timer(timer t,
		      unsigned int start_hi, unsigned int start_lo, 
		      unsigned int end_hi, unsigned int end_lo);


extern void rtcg_stat_output(FILE f, Cstring prog_name);
extern void rtcg_stat_genblock(int insts_gend);
