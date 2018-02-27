
#include <stdio.h>
#include <stdlib.h>

typedef long long int64;
typedef unsigned long long uint64;

typedef struct {
  int64 min; /* Least logged value. */
  int64 max; /* Least logged value. */
  int64 cum; /* Cumulative clock cycles. */
  int64 num; /* Number of times counter was logged. */
} timer_s, *timer_pt;

timer_s gc_timer_s = {0, };
timer_pt gc_timer = &gc_timer_s;

timer_s codegen_timer_s = {0, };
timer_pt codegen_timer = &codegen_timer_s;


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

static int64 allocated_regions; /* In bytes */
static int64 generated_code; /* bytes of x86 code generated. */
static int64 generated_funs; /* number of generated functions. */

void rtcg_stat_codegen(int code_size, int region_size) {
  allocated_regions += region_size;
  generated_code += code_size;
  generated_funs++;
}

void rtcg_stat_output(FILE *f,char *flags, char *name) {
  int i;

  fprintf(f,"flags=\'%s'\;\n",flags);
  fprintf(f,"generated_funs  = %12lld;\n",generated_funs);
  fprintf(f,"region_bytes    = %12lld;\n",allocated_regions);
  fprintf(f,"generated_bytes = %12lld;\n",generated_code);
  fprintf(f,"\n");

  output_timer(f,gc_timer,"gc_timer");
  output_timer(f,codegen_timer,"codegen_timer");

}
