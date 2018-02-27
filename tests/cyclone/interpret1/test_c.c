#include "opcodes.h"
#include "bench.h"

struct inst
{
  int opcode;
  int val;
};

extern void c_interpret(struct inst c_prg[]);
extern void c_interpret_opt(struct inst c_prg[]);
extern int int_time();
extern int print_time(int, int, int);

extern struct inst cprog1[],cprog2[],cprog3[],cprog4[],cprog5[];

int mem[4], stack[10], prg_size;
void init_state(), dump_mem();

void test_c(int bench, int pn, int iterations)
{
  struct inst *cprog;
  int start_time, end_time;
  int i;

  switch(pn) {
  case 1: cprog = cprog1; break;
  case 2: cprog = cprog2; break;
  case 3: cprog = cprog3; break;
  case 4: cprog = cprog4; break;
  case 5: cprog = cprog5; break;
  default: printf("Unrecognized program.\n"); exit(1);
  }

  for(prg_size = 0 ; cprog[prg_size].opcode != END ; prg_size++)
    ;
  printf("Program size: %d\n", prg_size);
  
  /* normal */
  printf("[cl (or gcc)]\n");

  init_state();			   /* initialize memory */
  if (bench == TEST) {

    dump_mem();		           /* verify memory contents */
    c_interpret(cprog);
    dump_mem();

  } else {

    start_time = int_time();
    for( i = 0; i < iterations; i++)
      c_interpret(cprog);		   /* execute C compiled function */
    end_time = int_time();
    print_time(start_time, end_time, 1);

  }
  printf("\n"); 

  /* optimized */
  printf("[cl /02 (or gcc -O3)]\n");
  
  init_state();			   /* initialize memory */
  if (bench == TEST) {

    dump_mem();		           /* verify memory contents */
    c_interpret(cprog);
    dump_mem();

  } else {

    start_time = int_time();
    for( i = 0; i < iterations; i++)
      c_interpret_opt(cprog);		   /* execute C compiled function */
    end_time = int_time();
    print_time(start_time, end_time, 1);

  }
  printf("\n"); 

}

/* initialize state */
void init_state()
{
  int i;
  
  mem[0]= 0;
  mem[1]= 10;
  mem[2]= 20;
  mem[3]= 30;

  /* stack[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0} */
  for(i = 0; i < 10; i++)
    stack[i]= 0;
}

/* print out memory contents */
void dump_mem()
{
  int i;

  for(i = 0 ; i < /* MEM_SIZE */4 ; i++) {
    printf("Memory: %d ", mem[i]);
  }
  printf("\n");
}
