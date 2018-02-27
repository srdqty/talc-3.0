/* TEMPO Version 1.194, 04/27/98, Copyright (c) IRISA/INRIA-Universite de Rennes */


/* 
call signature of entry point interpret:
binding times of read non_locals: prg_size S, mem D, inst.val S, stack D, 
  inst.opcode S 
residual non-locals bottom:  stack 
 */
struct inst { int opcode;
              int val; };
extern int mem[];
extern stack[];

void _Ginterpret_1_0_0();
void _Ginterpret_1_0_3();
void interpret_cts_5();

/*
void print_mem()
{
  int i;

  for(i = 0 ; i < 4 ; i++)
    printf("mem[%d]: %d  ", i, mem[i]);
  printf("\n");
}

main()
{
  print_mem();
  _Ginterpret_1_0_0();
  print_mem();

  mem[0] = 0;
  mem[1] = 6;
  print_mem();
  interpret();
  print_mem();
}
*/

void interpret_cts_5()
  {
    stack[1] = 1;
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    _Ginterpret_1_0_3();
    return;
  }

void _Ginterpret_1_0_3()
  {
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (! stack[1])
      {
        stack[1] = 0;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[2] = mem[stack[2]];
        stack[1] = stack[2] * stack[1];
        stack[2] = 0;
        mem[stack[2]] = stack[1];
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 1;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_3();
      }
    return;
  }

void _Ginterpret_1_0_0()
  {
    stack[1] = 1;
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (! stack[1])		/* JEQ 23 */
      {
        stack[1] = 0;		/* fall-thru */
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[2] = mem[stack[2]];
        stack[1] = stack[2] * stack[1];
        stack[2] = 0;
        mem[stack[2]] = stack[1];
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 1;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_3();	/* JMP 3 */
      }
    return;
  }
