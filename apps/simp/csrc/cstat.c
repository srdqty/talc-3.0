
#include <stdio.h>
#include <stdlib.h>

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
#define NUM_FLAGS F_FPCOND << 1
#define NUM_INSTS 0xff

#define MAX_BLOCK_LEN 128

typedef long long int64;
typedef unsigned long long uint64;

typedef struct {
  int64 min; /* Least logged value. */
  int64 max; /* Least logged value. */
  int64 cum; /* Cumulative clock cycles. */
  int64 num; /* Number of times counter was logged. */
} timer_s, *timer_pt;

int64 inst_counts[NUM_INSTS];
int64 iflag_counts[NUM_FLAGS]; /* == 128K entries * 8 bytes == 1 MB */
int64 instruction_count = 0;
int64 basic_blocks[MAX_BLOCK_LEN + 1];

int pages_allocated=0; /* Number of pages the program allocates.  Includes text/data/ and stack.*/ 

timer_s syscall_timer_s = {0,};
timer_pt syscall_timer = &syscall_timer_s;

timer_s gc_timer_s = {0, };
timer_pt gc_timer = &gc_timer_s;

timer_s codegen_timer_s = {0, };
timer_pt codegen_timer = &codegen_timer_s;

static int bb = 0;

void stat_op_tick(int mask, int flag) {
  bb++;
  instruction_count++;
  inst_counts[mask]++;
  iflag_counts[flag]++;
  if((flag & F_CTRL) != 0) {
    if(bb >= MAX_BLOCK_LEN) basic_blocks[MAX_BLOCK_LEN]++;
    else basic_blocks[bb]++;
    bb = 0;
  }
}

static unsigned int text_segment_size;
static unsigned int data_segment_size;
static unsigned int initial_stack_size;

void stat_segment_info(int text, int data, int stack) {
  text_segment_size = text;
  data_segment_size = data;
  initial_stack_size = stack;
}

void stat_alloc_pages(int n) {
  pages_allocated += n;
}

/* Desired output:
   1. Basic block size distribution.
   2. Number of instructions executed.
   3. Percentage of instructions in the following classes:
      a) integer instructions
      b) floating point instructions
      c) reads
      d) writes
      e) control flow instructions

   We generate matlab compatible output.
*/

void stat_timer(timer_pt p, unsigned int start_hi, unsigned int start_lo,
	       unsigned int   end_hi, unsigned int   end_lo) {

  uint64 start = (uint64)((((uint64)start_hi) << 32) | start_lo);
  uint64   end = (uint64)((((uint64)  end_hi) << 32) |   end_lo);

  int64  diff = end - start;

  if(p->num == 0) {
    p->min = diff;
    p->max = diff;
  } else {
    if(p->min>diff) p->min = diff;
    if(p->max<diff) p->max = diff;
  }

  p->cum += diff;
  p->num++;

  return;
}

static void output_timer(FILE *f, timer_pt t,char *name) {
  fprintf(f,"%% Timer = min, max, cumulative, number of counts\n");
  fprintf(f,"%s = [%12lld %12lld %12lld %12lld];\n\n",
	  name,
	  t->min,
	  t->max,
	  t->cum,
	  t->num);
}

void stat_output(FILE *f,char *name) {
  int i;
  int64 int_op=0,fp_op=0,rd_op=0,wr_op=0,ctrl_op=0,mem_op=0,call_op=0,trap_op=0;
  int64 num_blocks = 0;

  fprintf(f,"bench=\'%s\';\n",name);
  fprintf(f,"dyn_insts = %12lld;\n",instruction_count);

  for(i = 0; i <= MAX_BLOCK_LEN; i++) {
    num_blocks += basic_blocks[i];
  }

  fprintf(f,"dyn_bb = %lld;\n",num_blocks);

  fprintf(f,"dyn_bb_dist = [");

  for(i=0; i<=MAX_BLOCK_LEN;i++) {
    fprintf(f,"\t%12lld\n",basic_blocks[i]); 
  }
  fprintf(f,"];\n\n");

  for(i=0; i < NUM_FLAGS;i++) {
    int64 cnt = iflag_counts[i];
    if(i & F_ICOMP)  int_op += cnt;
    if(i & F_FCOMP)   fp_op += cnt;
    if(i & F_CTRL ) ctrl_op += cnt;
    if(i & F_TRAP)  trap_op += cnt;
    if(i & F_LOAD )   rd_op += cnt;
    if(i & F_STORE)   wr_op += cnt;
    if(i & F_MEM  )  mem_op += cnt;
    if(i & F_CALL ) call_op += cnt;
  }
    
  fprintf(f,"%% dyn_inst_kind_dist == dynamic instruction kind distribution.\n");
  fprintf(f,"%% in order the elements are int,fp,ctrl,trap,mem,call,rd,wr instructions.\n");
  fprintf(f,"dyn_inst_kind_dist =[");
  fprintf(f,"%12lld\n",int_op);
  fprintf(f,"%12lld\n",fp_op);
  fprintf(f,"%12lld\n",ctrl_op);
  fprintf(f,"%12lld\n",trap_op);
  fprintf(f,"%12lld\n",mem_op);
  fprintf(f,"%12lld\n",call_op); 
  fprintf(f,"%12lld\n",rd_op);
  fprintf(f,"%12lld\n",wr_op);
  fprintf(f,"];\n\n");

  fprintf(f,"%% dyn_inst_dist == dynamic instruction distribution.\n");
  fprintf(f," dyn_inst_dist = [ ");
  for(i = 0; i < NUM_INSTS; i++) {
    fprintf(f,"%12lld\n",inst_counts[i]);
  }
  fprintf(f,"];\n\n");

  fprintf(f,"%% Size of text and data segement (in bytes)\n");
  fprintf(f,"segments = [ %u %u ];\n",text_segment_size,data_segment_size);
  fprintf(f,"%% Initial stack size.\n");
  fprintf(f,"stack = %u;\n",initial_stack_size);
  fprintf(f,"%% Total number of pages allocates over lifetime of program.\n");
  fprintf(f,"%% Pages are 64KB each.\n");
  fprintf(f,"pages = %u;\n\n",pages_allocated);

  output_timer(f,syscall_timer,"syscall_timer");
  output_timer(f,gc_timer,"gc_timer");

  fclose(f);
}

static int64 allocated_regions; /* In bytes */
static int64 generated_code; /* bytes of x86 code generated. */
static int64 generated_funs; /* number of generated functions. */

void rtcg_stat_codegen(int code_size, int region_size) {
  allocated_regions += region_size;
  generated_code += code_size;
  generated_funs++;
}

static int64 generated_insts;
static int64 gen_basic_blocks[MAX_BLOCK_LEN + 1];

void rtcg_stat_genblock(int block_size) {
  // Size is in number of instructions.
  generated_insts += block_size;

  if(block_size >= MAX_BLOCK_LEN) {
    gen_basic_blocks[MAX_BLOCK_LEN]++;
  }
  else {
    gen_basic_blocks[block_size]++;
  }
}

void rtcg_stat_output(FILE *f,char *name) {
  int i;

  fprintf(f,"bench=\'%s\';\n",name);
  fprintf(f,"generated_funs  = %12lld;\n",generated_funs);
  fprintf(f,"region_bytes    = %12lld;\n",allocated_regions);
  fprintf(f,"generated_bytes = %12lld;\n",generated_code);
  fprintf(f,"generated_insts = %12lld;\n",generated_insts);
  fprintf(f,"\n");

  fprintf(f,"%% Distribution of generated blocks.\n");
  fprintf(f,"gen_bb_dist = [ ");
  for(i = 0; i <= MAX_BLOCK_LEN; i++) {
    fprintf(f,"\t%12lld\n", gen_basic_blocks[i]);
  }
  fprintf(f,"];\n\n");

  output_timer(f,syscall_timer,"syscall_timer");
  output_timer(f,gc_timer,"gc_timer");
  output_timer(f,codegen_timer,"codegen_timer");

}
