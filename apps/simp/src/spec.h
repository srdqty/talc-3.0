// Machine specifications used pervasively throughout

#ifndef __SPEC_H
#define __SPEC_H

// typedefs
#define word unsigned int
#define ptr unsigned int

// Memory organization
#define TEXT_BASE  0x00400000
#define DATA_BASE  0x10000000
#define STACK_BASE 0x7fffc00

#define PAGE_SIZE  4096

/* rounding macros, assumes ALIGN is a power of two */
#define ROUND_UP(N,ALIGN)	(((N) + ((ALIGN)-1)) & ~((ALIGN)-1))
#define ROUND_DOWN(N,ALIGN)	((N) & ~((ALIGN)-1))

// Machine architecture
#define NUM_BASE_REGS 32
#define NUM_MISC_REGS 6 // HI + LO + FCC + TMP + MEM + CTRL
#define TOTAL_REGS (NUM_BASE_REGS + NUM_BASE_REGS + NUM_MISC_REGS)

#define R_GP  28
#define R_SP  29
#define R_FP  30

/* pre/post-incr/decr operation field specifiers */
#define COMP_NOP		0x00
#define COMP_POST_INC	        0x01
#define COMP_POST_DEC	        0x02
#define COMP_PRE_INC		0x03
#define COMP_PRE_DEC		0x04
#define COMP_POST_DBL_INC	0x05	/* for double word accesses */
#define COMP_POST_DBL_DEC	0x06
#define COMP_PRE_DBL_INC	0x07
#define COMP_PRE_DBL_DEC	0x08

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

/* lwl/swl defs */
#define SS_LR_MASKS(X)      ((~0) >>> ((X) * 8))

#define WL_SIZE(ADDR)       (4-((ADDR) & 0x03))
#define WL_BASE(ADDR)       ((ADDR) & ~0x03)
#define WL_PROT_MASK(ADDR)  SS_LR_MASKS(4 - WL_SIZE(ADDR))
#define WL_PROT_MASK1(ADDR) SS_LR_MASKS(WL_SIZE(ADDR))
#define WL_PROT_MASK2(ADDR) SS_LR_MASKS(4 - WL_SIZE(ADDR))

/* lwr/swr defs */
#define WR_SIZE(ADDR)       (((ADDR) & 0x03)+1)
#define WR_BASE(ADDR)       ((ADDR) & ~0x03)
#define WR_PROT_MASK(ADDR)  (~(SS_LR_MASKS(WR_SIZE(ADDR))))
#define WR_PROT_MASK1(ADDR) SS_LR_MASKS(WR_SIZE(ADDR))
#define WR_PROT_MASK2(ADDR) SS_LR_MASKS(4 - WR_SIZE(ADDR))

/* #define WR_PROT_MASK(ADDR)  (~(ss_lr_masks[WR_SIZE(ADDR)])) */

/* largest signed integer */
#define MAXINT_VAL	0x7fffffff

// As far as I can tell we don't need this but the SimpleScalar Toolset 
// never allocates a stack smaller than this.
#define MIN_STACK_SIZE 16384

// Size in bytes
#define INST_SIZE 8

#endif // EOF spec.h


