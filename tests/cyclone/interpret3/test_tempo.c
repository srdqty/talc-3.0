#include "opcodes.h"
#include "bench.h"
#include <stdio.h>

struct inst
{
  int opcode;
  int val;
};

/* #include "interpret.rts.h" */

extern int unix_time();
extern int print_time(int, int, int);
extern void init_state();

int mem[4], stack[10], prg_size;
void init_state(), dump_mem();

void test_cts(int bench, void cts_interp(), int iterations)
{
  int start_time, end_time;
  int i;

  /* normal */
  printf("[Tempo CTS]\n");

  init_state();			   /* initialize memory */
  if (bench == TEST) {

    dump_mem();		           /* verify memory contents */
    cts_interp();
    dump_mem();

  } else {

    start_time = unix_time();
    for( i = 0; i < iterations; i++)
      cts_interp();
    end_time = unix_time();
    print_time(start_time, end_time, 1);
    
  }
  printf("\n"); 
}

void test_rts(int bench,
	      void *f(struct inst *, int, int),
	      struct inst cprog[],
	      int iterations)
{
  int start_time, end_time, i;
  void (* rts_spec)();

  /* determine length of program here */
  for(prg_size = 0 ; cprog[prg_size].opcode != END ; prg_size++)
    ;
  printf("Program size: %d\n", prg_size);

  printf("[Tempo RTS]\n");

  if (bench == TEST) {

    rts_spec = (*f)(cprog, 0, 0); /* specialize function */
    init_state();		             /* initialize memory  */
    dump_mem();			       /* verify memory contents */
    (*rts_spec)();
    dump_mem();

  } else {
    
    /* Can't currently measure code generation time */
    rts_spec = (*f)(cprog, 0, 0);
    /*
    start_time = unix_time();
    for( i = 0; i < iterations / 10; i++)
      rts_spec = rts_interpret_1(cprog, 0, 0);
    end_time = unix_time();
    printf("Code generation (NOTE: * 10 !!!):\n");
    print_time(start_time, end_time, 1);
    */
    
    start_time = unix_time();
    for( i = 0; i < iterations; i++)
      (* rts_spec)();	                 /* execute specialized function */
    end_time = unix_time();
    printf("Code execution:\n");
    print_time(start_time, end_time, 1);
  }
  printf("\n"); 
}
