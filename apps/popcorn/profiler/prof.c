#include "prof.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/******************************************************************************/
/* Defines */

#ifdef __linux__
#define pLD "ll"
#else
#define pLD "I64"
#endif

#ifdef __linux__
#define GET_RDTSC(X) __asm__ __volatile__("pushal\n"\
				  "\tcpuid\n"\
				  "\trdtsc\n"\
				  "\tmovl %%edx,%0\n"\
				  "\tmovl %%eax,%1\n"\
				  "\tpopal\n" : "=m" (X##_hi), "=m" (X##_lo) );\
X = (int64)((((uint64)X##_hi) << 32) | X##_lo)
#else
#define RDTSC __asm _emit 0fh __asm _emit 031h
/* INTEL recommends using CPUID to force serialization. */
#define CPUID __asm _emit 0fh __asm _emit 0a2h
#define GET_RDTSC(X) __asm \
{                          \
   __asm pushad            \
   CPUID                   \
   RDTSC                   \
   __asm mov X##_hi, edx   \
   __asm mov X##_lo, eax   \
   __asm popad             \
}                          \
X = (int64)((((uint64)X##_hi) << 32) | X##_lo)
#endif

#define STACKLET_SIZE (1 << 14)

/******************************************************************************/
/* Type declarations */

#ifdef __linux__
typedef unsigned long long int uint64;
#else 
typedef unsigned __int64 uint64;
#endif

typedef unsigned uint32;
typedef int int32;

typedef struct {
  prof_t p;
  int64 within; /* Calls made within this call. */
} stack_elt;

typedef struct stk_s {
  stack_elt elts[STACKLET_SIZE];
  struct stk_s *prev;
} stack_s, *stack_t;

/******************************************************************************/
/* Globals */

static stack_t stack = NULL;
static stack_elt *sp;

static prof_s dummy_prof  = {0,0,0,0,0,0,"BOGUS$@$"};
static prof_s dummy_prof2 = {0,0,0,0,0,0,"BOGUS$@$"};
static int64 start_tsc;
char *prof_outfile = "fmon.data";
char *prof_outfile_comprehensive = "fmon_all.data";

/******************************************************************************/
/* Utilities */

double percent_error(int64 error, int64 base) {
  return 100.0 * ((double)error/(double)base);
}

void prof_clear(prof_t p) {
  p->cycles = 0;
  p->child_cycles = 0;
  p->within_count = 0;
  p->outermost_count = 0;
  p->outermost_cycles = 0;
  p->depth = 0;
}

/******************************************************************************/
/* Stack functions and macros*/

#define PUSH(X) if(++sp < (stack->elts + STACKLET_SIZE)) { sp->p = (X); sp->within = 0; }\
                else push_stacklet(X);
#define POP if(--sp < stack->elts) pop_stacklet();

void new_stacklet() {
  stack_t x = (stack_t)malloc(sizeof(stack_s));
  if(x == NULL) {
    fprintf(stderr,"Profiler: failed to allocate memory.\n");
    exit(1);
  }
  x->prev = stack;
  stack = x;
}

/* When we really have to pop the top of the stack.  This works in conjunction
   with the POP macro. */
void pop_stacklet() {
  if(stack->prev == NULL) {
    fprintf(stderr,"IMPOSSIBLE: Popped the very bottom of the stack.\n");
    exit(1);
  }
  stack = stack->prev;
  sp = &(stack->elts[STACKLET_SIZE-1]);
}

/* When we have to allocate a new stacklet. */
/* Works in conjunction with PUSH macro. */
void push_stacklet(prof_t p) {
  new_stacklet();
  sp = &(stack->elts[0]);
  sp->p = p;
  sp->within = 0;
}

/******************************************************************************/
/* Hashtable */

/* We wish to avoid allocation except infrequently, and keep the running time
   constant.

   Therefore:
   1. Inline the data structures into the hash table
   2. Use double hashing to resolve collisions
   3. When necessary resize the table.

   The hash function must not be commutative otherwise (a calls b) will 
   always collide with (b calls a).

*/
typedef struct he_t {
  prof_t parent, child;
  int64 count;
} hash_elt_s;

typedef struct a_s {
  struct a_s *next;
  prof_t elt;
  int64 count;
} adj_s;

typedef struct {
  adj_s *callers;
  adj_s *callees;
} node_summary_s;

typedef struct {
  hash_elt_s *elts;
  int size;     /* size (prime number) */
  int exp;      /* exponent used to determine the size. */
  int num_elts; /* number of elements in the hash table. */
} hash_table_s;

static int hash_insert(prof_t parent, prof_t child);

/* 2 ** n - prime_diff[n]  is a prime! */
int prime_diff[32] = { 1, 2, 3, 1,
		       3, 1, 3, 1,
		       5, 3, 3, 9,
		       3, 1, 3,19,
		      15, 1, 5, 1,
		       3, 9, 3,15,
		       3,39, 5,39,
		      57, 3,35, 1};

/* A global hash table. */
hash_table_s hash_table;
int32 hash_collisions;
int32 hash_insertions;
int32 hash_initial_size;

/* May want to inline the hash. */
static unsigned prof_hash(prof_t parent, prof_t child) {
  unsigned p1 = (unsigned)parent, p2 = (unsigned)(child);
  int sz = hash_table.size;
  unsigned p1_swap = (((unsigned)p1) >> 16) ^ (p1 << 16);
  return ((unsigned)(p1_swap ^ p2)) % sz; 
}

static void hash_alloc(int exp) {
  int i;

  if(exp<0 || exp>=32) {
    fprintf(stderr,"Cannot allocate a hash table of size %d.\n",exp);
    exit(1);
  }

  hash_table.exp = exp;
  hash_table.size = (1 << hash_table.exp) - prime_diff[hash_table.exp];
  hash_table.elts = (hash_elt_s *)malloc(sizeof(hash_elt_s) * hash_table.size);
  hash_table.num_elts = 0;

  if(hash_table.elts==NULL) {
    fprintf(stderr,"Failed to allocate hash table of size %d.\n",
	    hash_table.size);
    exit(1);
  }

 /* Using bzero would be faster, but who cares! */
  for(i=0; i<hash_table.size; i++) {
    hash_elt_s *h = &hash_table.elts[i];
    h->parent= NULL;
    h->child = NULL;
    h->count = 0;
  }
}

static void hash_init(int num_funs) {
  int i=0;
  int estimated_entries = num_funs * 113; /* Purposefully guess large to cause few collisions. */

  for(i=31; i>=0; i--) {
    if((estimated_entries & (1<<i)) != 0) break;
  }

  hash_alloc(i+1);
  hash_initial_size = hash_table.size;
}

static void hash_extend() {
  int i;
  hash_table_s h_old = hash_table;

  hash_alloc(hash_table.exp + 1);
  hash_table.num_elts = 0;

  for(i=0; i<h_old.size; i++) {
    if(h_old.elts[i].parent!=NULL) {
      int pos = hash_insert(h_old.elts[i].parent,h_old.elts[i].child);
      if(pos<0 || pos >= hash_table.size) {
	fprintf(stderr,
		"Hash table insert failed during extension (pos = %d).\n",pos);
	exit(1);
      }
      hash_table.elts[pos].count = h_old.elts[i].count;
    }
  }

  free(h_old.elts);
}

static int hash_insert(prof_t parent, prof_t child) {
  int v = prof_hash(parent,child);
  int sz,collision=0;
  hash_elt_s *h;

  hash_insertions++;

  if(v>=hash_table.size || v <0) {
    printf("Inserting %s calls %s at %d\n",parent->name,child->name,v);
    exit(1);
  }

  if(hash_table.num_elts > (hash_table.size >> 1)) { 
    printf("Extending the hash table from %d, %d\n",hash_table.size,hash_table.num_elts);
    fflush(stdout);
    hash_extend();
    v = prof_hash(parent,child);
  }

  /* First attempt uses linear probing.  The problem with this is that
     there is a systematic cost for an edge that has a collision.  
     If this edge is heavily executed then we always pay this cost, but
     we don't account for it properly in the profiler's overhead.
  */
  sz = hash_table.size;
  h = &hash_table.elts[v];
  while(1) {
    if(h->parent == parent && h->child == child) { 
      h->count++;
      if(collision) hash_collisions++;
      return v; 
    }
    if(h->parent == NULL) {
      h->parent = parent;
      h->child = child;
      h->count = 1;
      hash_table.num_elts++;
      if(collision) hash_collisions++;
      return v;
    }
    
    v=(v+1) % sz;
    h=&hash_table.elts[v];
    collision=1;
  }
}

static adj_s *new_adj(prof_t n,int64 c,adj_s *a) {
  adj_s *t = (adj_s *)malloc(sizeof(adj_s));

  if(t==NULL) {
    fprintf(stderr,"Failed to allocate adjacency struct.\n");
    exit(1);
  }

  t->elt = n;
  t->next = a;
  t->count = c;

  return t;
}

static adj_s **adj_to_array(adj_s *a, int *len_rtn) {
  int len,i;
  adj_s **arr, *lst;

  lst = a;
  for(len=0;lst!=NULL;lst=lst->next) {
    len++;
  }
  
  arr = (adj_s **)malloc(sizeof(adj_s *) * len);

  for(i=0;i<len;i++) {
    arr[i] = a;
    a = a->next;
  }

  *len_rtn = len;
  return arr;
}

static int cmp_adj_t(adj_s **a, adj_s **b) {
  int64 c1 = (*a)->count;
  int64 c2 = (*b)->count;

  if(c1>c2) return -1;
  else if (c1<c2) return 1;
  else return 0;
}

static node_summary_s hash_summarize(prof_t node) {
  int i;
  node_summary_s n = {0,0};

  for(i=0; i < hash_table.size;i++) {
    hash_elt_s *h = &hash_table.elts[i];
    if(h->parent == node)
      n.callees = new_adj(h->child,h->count,n.callees);
    if(h->child == node) 
      n.callers = new_adj(h->parent,h->count,n.callers);
  }

  return n;
}


static void output_adj(FILE *f,adj_s *a, int32 fname_width) {
	adj_s **arr;
	int i,len;
	arr = adj_to_array(a,&len);
	qsort((void *)arr,len,sizeof(adj_s *),(int (*)(const void*,const void*))cmp_adj_t);
	
	for(i=0;i<len;i++) {
	  fprintf(f,"   %*s %12"pLD"d\n",fname_width,arr[i]->elt->name,arr[i]->count);
	}
}


static void output_adj_comprehensive(FILE *f,adj_s *a) {
	adj_s **arr;
	int i,len;
	arr = adj_to_array(a,&len);
	qsort((void *)arr,len,sizeof(adj_s *),(int (*)(const void*,const void*))cmp_adj_t);
	
	for(i=0;i<len;i++) {
	  fprintf(f,",%s %"pLD"d\n",arr[i]->elt->name,arr[i]->count);
	}
}

/******************************************************************************/
/* Calibrate the timer */
/* Copied from Intel's sample code. */
#ifdef __linux__
uint32 find_base() {
  uint32 cycles_low=0, cycles_high=0;
  uint32 base=0,base_extra=0;
 
  __asm__ __volatile__ ("\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"			
			"\tmovl %%edx,%3\n"
			"\tmovl %%eax,%2\n"
			"\tpopal\n"
			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tpopal\n"

			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tmovl %%edx,%3\n"
			"\tmovl %%eax,%2\n"
			"\tpopal\n"
			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tpopal\n"

			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tmovl %%edx,%3\n"
			"\tmovl %%eax,%2\n"
                        "\tpopal\n"
			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tsubl %2,%%eax\n"
			"\tmovl %%eax,%1\n"
			"\tpopal\n"


			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			"\tmovl %%edx,%3\n"
			"\tmovl %%eax,%2\n"
			"\tpopal\n"
			"\tpushal\n"
			"\tcpuid\n"
			"\trdtsc\n"
			
			"\tsubl %2,%%eax\n"
			"\tmovl %%eax, %0 \n"
			"\tpopal\n" 
			: "=m" (base), "=m" (base_extra), 
			  "=m" (cycles_low), "=m" (cycles_high) 
			: 
			: "ax","bx","cx","dx" );
  if(base_extra < base) base = base_extra;

  return base;
}

#else 
uint32 find_base() {
  uint32 cycles_high,cycles_low;
  uint32 base=0,base_extra=0;
 
  __asm {
    __asm pushad
          CPUID
          RDTSC
    __asm mov cycles_high,edx
    __asm mov cycles_low,eax
    __asm popad
    __asm pushad
          CPUID
          RDTSC
    __asm popad

    __asm pushad
          CPUID
          RDTSC
    __asm mov cycles_high,edx
    __asm mov cycles_low,eax
    __asm popad
    __asm pushad
          CPUID
          RDTSC
    __asm popad

    __asm pushad
          CPUID
          RDTSC
    __asm mov cycles_high,edx
    __asm mov cycles_low,eax
    __asm popad
    __asm pushad
          CPUID
          RDTSC
    __asm sub eax,cycles_low
    __asm mov base_extra, eax
    __asm popad

    __asm pushad
          CPUID
          RDTSC
    __asm mov cycles_high,edx
    __asm mov cycles_low,eax
    __asm popad
    __asm pushad
          CPUID
          RDTSC
    __asm sub eax,cycles_low
    __asm mov base,eax
    __asm popad
      }
  if(base_extra < base) base = base_extra;

  return base;
}
#endif

/******************************************************************************/
/* Profiler functions */

#define NUM_TRIALS 50

int cmp_int32(int32 *x, int32 *y) {
  return *x - *y;
}


/******************************************************************************/
/* Profiler aggregation functions */

typedef struct {
  prof_t p;
  int64 caller_count, callee_count, within_count,outermost_count; /* Counts. */
  int64 uncorrected_self_cycles, uncorrected_total_cycles;
  int64 corrected_self_cycles, corrected_total_cycles;
  adj_s *callees;
  adj_s *callers;
} fun_stat_s;

typedef struct {
  int32 fname_width; /* Maximum width function name. */
  int64 uncorrected_total_cycles; /* Measured at profiler exit. */
  int64 corrected_total_cycles;
  int64 total_calls;
  fun_stat_s **funs;
  int32 num_funs;
  /* Costs are used to correct values.  In the future may want something
     more sophisticated here. */
  int32 base_cost; /* Minimum cycles the timer will ever report. */
  int32 null_cost; /* Time taken by the profiler to enter & leave. */
  int32 interior_cost; /* Time reported by the profiler for a null function. */
} file_stat_s;

// Compute the corrected values from the reported times.
void prof_costs(file_stat_s *file_stat) {
  uint32 null_start_lo,null_start_hi,null_end_lo,null_end_hi;
  int64  null_start,null_end;
  int32  null_cycles[NUM_TRIALS],interior_cycles[NUM_TRIALS];
  int32  base = find_base();
  int32  i;
  int32 null_cost     = 0;
  int32 interior_cost = 0;
  file_stat->base_cost = base;
  
  
  // Warm up the cache!
  prof_enter(&dummy_prof2);
  prof_leave(&dummy_prof2);

  prof_enter(&dummy_prof2);
  prof_leave(&dummy_prof2);

  prof_enter(&dummy_prof2);
  prof_leave(&dummy_prof2);

  prof_enter(&dummy_prof2);
  prof_leave(&dummy_prof2);

  // Over-estimating the null_cost may cause negative readings
  // Under-estimating the null-cost artificially inflates the
  // importance of functions containing many function calls
  // Over-estimating the interior-cost causes the times for frequently
  // called functions to be underreported.
  // We go with an average here.
  for(i = 0; i < NUM_TRIALS; i++) {

    dummy_prof2.cycles          = 0;
    dummy_prof2.child_cycles    = 0;
    dummy_prof2.within_count    = 0;
    dummy_prof2.outermost_count = 0;
    dummy_prof2.outermost_cycles= 0;
    dummy_prof2.depth           = 0;

    GET_RDTSC(null_start);
    prof_enter(&dummy_prof2);
    prof_leave(&dummy_prof2);

    prof_enter(&dummy_prof2);
    prof_leave(&dummy_prof2);

    prof_enter(&dummy_prof2);
    prof_leave(&dummy_prof2);

    GET_RDTSC(null_end);

    null_cycles[i] = (null_end - null_start - base)/3;
    interior_cycles[i] = dummy_prof2.cycles/3;    
  }

  qsort((void *)null_cycles,NUM_TRIALS,sizeof(int32),(int (*)(const void*,const void*))cmp_int32);
  qsort((void *)interior_cycles,NUM_TRIALS,sizeof(int32),(int (*)(const void*,const void*))cmp_int32);
  null_cost = 0;
  interior_cost = 0;
  for(i=(NUM_TRIALS/10); i < NUM_TRIALS-(NUM_TRIALS/10); i++) {
    null_cost += null_cycles[i];
    interior_cost += interior_cycles[i];
  }
  null_cost     = null_cost / (NUM_TRIALS - 2 * (NUM_TRIALS/10));
  interior_cost = interior_cost / (NUM_TRIALS - 2 * (NUM_TRIALS/10));

  file_stat->null_cost = null_cost;
  file_stat->interior_cost = interior_cost;
}

void prof_aggregate(file_stat_s *file_stat) {
  int i;
  fun_stat_s *stats = (fun_stat_s *)malloc(sizeof(fun_stat_s) * prof_data_size);
  fun_stat_s **funs = (fun_stat_s **)malloc(sizeof(fun_stat_s *) * prof_data_size);
  int64 total_calls = 0;
  int32 fname_width = 0;
  if(stats == NULL || funs == NULL) {
    fprintf(stderr,"Profiler: failed to allocate memory for statistics.\n");
    exit(1);
  }

  for(i=0; i<prof_data_size; i++) {
    fun_stat_s *s = &stats[i];
    prof_t p = prof_data[i];
    node_summary_s n = hash_summarize(p);
    adj_s *callees = n.callees;
    adj_s *callers = n.callers;
    int64 callee_count,caller_count;
    int32 pname_width = strlen(p->name);

    funs[i] = s;

    if(pname_width>fname_width) fname_width = pname_width;

    for(callee_count=0;callees!=NULL;callees=callees->next) {
      callee_count += callees->count;
    }

    for(caller_count=0;callers!=NULL;callers=callers->next) {
      caller_count += callers->count;
    }

    total_calls += callee_count;

    s->p                        = p;
    s->caller_count             = caller_count;
    s->callee_count             = callee_count;
    s->within_count             = p->within_count;
    s->outermost_count          = p->outermost_count;
    s->uncorrected_self_cycles  = p->cycles - p->child_cycles;
    s->uncorrected_total_cycles = p->outermost_cycles;
    s->callees                  = n.callees;
    s->callers                  = n.callers;
  }

  file_stat->total_calls = total_calls +1; /* +1 is for main that is not called by anyone. */
  file_stat->funs        = funs;
  file_stat->num_funs    = prof_data_size;
  file_stat->fname_width = fname_width;
}

/* Compute all the corrected results. */
void prof_correct(file_stat_s *file_stat) {
  int32 i;
  int32 num_funs = file_stat->num_funs;
  fun_stat_s **funs = file_stat->funs;
  int32 interior_cost = file_stat->interior_cost;
  int32 null_cost = file_stat->null_cost;
  int32 exterior_cost = null_cost - interior_cost;

  for(i=0;i<num_funs;i++) {
    fun_stat_s *f = funs[i];

    f->corrected_self_cycles = (f->uncorrected_self_cycles 
				- (f->caller_count * interior_cost) 
				- (f->callee_count * exterior_cost));

    f->corrected_total_cycles = (f->uncorrected_total_cycles 
				 - (f->within_count * null_cost) 
				 - (f->outermost_count * interior_cost));
  }

  file_stat->corrected_total_cycles = 
    file_stat->uncorrected_total_cycles - (file_stat->total_calls * null_cost);

}

/******************************************************************************/
/* Profiler output functions. */

/* Later we should fancify this with percentages and blah blah.... */
static void output_fun(FILE *f, fun_stat_s * fun,
		       int64 total,int32 fname_width) {
  char *name           = fun->p->name;
  int64 caller_count   = fun->caller_count;
  int64 callee_count   = fun->callee_count;
  int64 self_cycles    = fun->corrected_self_cycles;
  int64 total_cycles   = fun->corrected_total_cycles;
  int64 child_cycles   = total_cycles - self_cycles;
  double self_percent  = percent_error(self_cycles,total);
  double total_percent = percent_error(total_cycles,total);

  /* Here we go to ridiculous lengths to get the format xx.xx% */
  int s_per = self_percent;
  int s_per2 = (int)(100.0 * self_percent) - (s_per * 100);
  int t_per = total_percent;
  int t_per2 = (int)(100.0 * total_percent) - (t_per * 100);

  if(t_per >= 100) { t_per = 99; t_per2 = 99; }

  fprintf(f,"%*s ",fname_width,name);
  fprintf(f," %10"pLD"d  %10"pLD"d  ",caller_count,callee_count);
  fprintf(f,"%12"pLD"d %2d.%02d%%  %12"pLD"d  ",total_cycles,t_per,t_per2,
	  child_cycles);
  fprintf(f,"%12"pLD"d %2d.%02d%%",self_cycles,s_per,s_per2);
  fprintf(f,"\n");
}

/* Output suitable for machine analysis. */
static void output_fun_comprehensive(FILE *f, fun_stat_s * fun) {
  char *name           = fun->p->name;
  int64 caller    = fun->caller_count;
  int64 callee    = fun->callee_count;
  int64 within    = fun->within_count;
  int64 outermost = fun->outermost_count;
  int64 raw_self_cycles  = fun->uncorrected_self_cycles;
  int64 self_cycles      = fun->corrected_self_cycles;
  int64 raw_total_cycles = fun->uncorrected_total_cycles;
  int64 total_cycles   = fun->corrected_total_cycles;

  fprintf(f,"%s,%"pLD"d, %"pLD"d, %"pLD"d, %"pLD"d",name,caller,callee,within,outermost);
  fprintf(f,", %"pLD"d, %"pLD"d, %"pLD"d, %"pLD"d\n",
	  raw_self_cycles, self_cycles, raw_total_cycles, total_cycles);
}

int cmp_fun_stat_t(fun_stat_s **f1, fun_stat_s **f2) {
  int64 c1 = (*f1)->corrected_self_cycles;
  int64 c2 = (*f2)->corrected_self_cycles;

  if(c1>c2) return -1;
  else if (c1<c2) return 1;
  else return 0;
}

/* Generate a summary of the collected statistics. */
/* Could be entering a top-level handler! */
void prof_output(FILE *fptr, file_stat_s *file_stat) {
  int64 overhead;
  fun_stat_s **funs = file_stat->funs;
  int32 num_funs = file_stat->num_funs;
  int32 fname_width = file_stat->fname_width + 1;
  int64 total_cycles = file_stat->corrected_total_cycles;
  int32 i;

  overhead = 
    file_stat->uncorrected_total_cycles - file_stat->corrected_total_cycles;

  /* Header */
  fprintf(fptr,"Total cycles: corrected = %"pLD"d raw = %"pLD"d\n",
	  file_stat->corrected_total_cycles,
	  file_stat->uncorrected_total_cycles);
  fprintf(fptr, "Total calls = %"pLD"d\n", 
	  file_stat->total_calls);
  fprintf(fptr,"Corrective factors: base cost = %d null cost = %d interior cost = %d\n",
	  file_stat->base_cost,
	  file_stat->null_cost,
	  file_stat->interior_cost);
  fprintf(fptr,"Estimated overhead = %"pLD"d (%2.2g%% of total)\n",
	  overhead,
	  percent_error(overhead,file_stat->uncorrected_total_cycles));
  fprintf(fptr,"Hash table: initial size = %d final size = %d elements = %d insertions = %d, collisions = %d (%.2g%%)\n",
	  hash_initial_size,hash_table.size,
	  hash_table.num_elts,hash_insertions,hash_collisions,
	  percent_error(hash_collisions,hash_insertions));

  qsort((void *)funs,num_funs,sizeof(fun_stat_s *),(int (*)(const void*,const void*))cmp_fun_stat_t);


  /* Output the running times. */
 fprintf(fptr,"%*s      Called       Calls     Cycles                 Child Cycles  Self Cycles\n",fname_width,"Function");
  for(i = 0; i < num_funs; i++) {
    if(funs[i]->caller_count != 0) 
      output_fun(fptr,funs[i],total_cycles,fname_width);
  }
  fprintf(fptr,"\n\n");

  /* Output the call graph. */
  for(i = 0; i < num_funs; i++) {
    if(funs[i]->callers != NULL) {
      fprintf(fptr,"%s is called by\n",funs[i]->p->name);
      output_adj(fptr,funs[i]->callers,fname_width);
    }
    if(funs[i]->callees != NULL) {
      fprintf(fptr,"%s calls\n",funs[i]->p->name);
      output_adj(fptr,funs[i]->callees,fname_width);
    }
  }
  fprintf(fptr,"\n");

}

/* Generate output suitable for data analysis in Excel. */
void prof_output_comprehensive(FILE *fptr, file_stat_s *file_stat) {
  int64 overhead;
  fun_stat_s **funs = file_stat->funs;
  int32 num_funs = file_stat->num_funs;
  int32 i;

  overhead = 
    file_stat->uncorrected_total_cycles - file_stat->corrected_total_cycles;

  /* Header */
  fprintf(fptr,"Corrected total cycles, %"pLD"d\n",file_stat->corrected_total_cycles);
  fprintf(fptr,"Raw total cycles, %"pLD"d\n"      ,file_stat->uncorrected_total_cycles);
  fprintf(fptr,"Total calls, %"pLD"d\n"           ,file_stat->total_calls);
  fprintf(fptr,"Base cost, %d\n"                  ,file_stat->base_cost);
  fprintf(fptr,"Null cost, %d\n"                  ,file_stat->null_cost);
  fprintf(fptr,"Interior cost, %d\n"              ,file_stat->interior_cost);
  fprintf(fptr,"Hash table size, %d\n"            ,hash_table.size);
  fprintf(fptr,"Hash Insertions, %d\n"            ,hash_insertions);
  fprintf(fptr,"Hash Collisions, %d\n"            ,hash_collisions); 

  qsort((void *)funs,num_funs,sizeof(fun_stat_s *),(int (*)(const void*,const void*))cmp_fun_stat_t);

  fprintf(fptr,"Function,Callers,Callees,Within,Outermost calls,Raw self,Self,Raw Total, Total\n");

  for(i = 0; i < num_funs; i++) {
    if(funs[i]->caller_count != 0) 
      output_fun_comprehensive(fptr,funs[i]);
  }
  fprintf(fptr,"\n\n");

  /* Output the call graph. */
  for(i = 0; i < num_funs; i++) {
    if(funs[i]->callers != NULL) {
      fprintf(fptr,"%s is called by\n",funs[i]->p->name);
      output_adj_comprehensive(fptr,funs[i]->callers);
    }
    if(funs[i]->callees != NULL) {
      fprintf(fptr,"%s calls\n",funs[i]->p->name);
      output_adj_comprehensive(fptr,funs[i]->callees);
    }
  }
  fprintf(fptr,"\n");

}

/******************************************************************************/
/* Core profiling functions */

/* Call on function entry. */
void prof_enter(prof_t p) {
  uint32 t_lo,t_hi;
  int64 t;
  stack_elt *top;

  GET_RDTSC(t);

  top = sp;
  PUSH(p);

  p->cycles      -= t;
  top->p->child_cycles -= t;

  if(p->depth==0) {
    p->outermost_cycles -= t;
  }

  p->depth++;
  hash_insert(top->p,p);
}

/* Call on function exit. */
void prof_leave(prof_t p) {
  uint32 t_lo,t_hi;
  int64 t;
  stack_elt* top, *old_top;

  GET_RDTSC(t);

  old_top = sp;
  POP;
  top = sp; /*We place a dummy initial top so there is always one.*/

  p->cycles            += t;
  top->p->child_cycles += t;

  p->depth--;
  top->within += old_top->within + 1;
  
  if(p->depth == 0) {
    p->within_count += old_top->within;
    p->outermost_count ++;
    p->outermost_cycles += t;
  }
}

/* Call on handler entry. */
void prof_handler(prof_t p) {
  while(sp->p != p) {
    prof_leave(sp->p);
  }
} 

/******************************************************************************/
/* Profiler entry and exit points. */

/* Initialize the profiler. */
void prof_init(){
  uint32 start_tsc_lo, start_tsc_hi; 
  int i = 0;

  hash_init(prof_data_size);

  for(i=0; i < prof_data_size; i++) {
    prof_t p = prof_data[i];
    p->cycles          = 0;
    p->child_cycles    = 0;
    p->within_count    = 0;
    p->outermost_count = 0;
    p->depth           = 0;
  }

  push_stacklet(&dummy_prof);

  /* Make sure prof_enter and prof_leave are in the cache!! */
  prof_enter(&dummy_prof2);
  prof_leave(&dummy_prof2);

  prof_clear(&dummy_prof);
  prof_clear(&dummy_prof2);

  GET_RDTSC(start_tsc);  
}

/* Could be entering a top-level handler! */
void prof_end() {
  uint32 end_tsc_lo, end_tsc_hi;
  int64 end_tsc,total_cycles;
  stack_elt *top;
  file_stat_s file_stat;
  FILE *fptr;

  top = sp;
  if(top->p != &dummy_prof)  /* If we ended in a handler. */
    prof_handler(&dummy_prof);

  GET_RDTSC(end_tsc);
 
  total_cycles = end_tsc - start_tsc;

  file_stat.uncorrected_total_cycles = total_cycles;
  prof_aggregate(&file_stat);

  prof_costs(&file_stat);
  prof_correct(&file_stat);

  fptr = fopen(prof_outfile,"w");
  if(fptr == NULL) {
    fprintf(stderr,"Failed to open output file %s for writing\n",prof_outfile);
    fflush(stderr);
    exit(1);
  }
  prof_output(fptr,&file_stat);

  fclose(fptr);

  fptr = fopen(prof_outfile_comprehensive,"w");
  if(fptr == NULL) {
    fprintf(stderr,"Failed to open output file %s for writing\n",prof_outfile_comprehensive);
    fflush(stderr);
    exit(1);
  }

  prof_output_comprehensive(fptr,&file_stat);

  fclose(fptr);
  free(file_stat.funs);
}





