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
extern int stack[];

void _Ginterpret_1_0_30o();
void _Ginterpret_1_0_15o();
void _Ginterpret_1_0_6o();
void interpret_cts_3();

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
}
*/

/* JMP 22 */
extern void _Ginterpret_1_0_30o/*50*/()
  {
    stack[1] = 3;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (stack[1])
      {
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])
          {
            stack[1] = 1;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 1;
            mem[stack[2]] = stack[1];
            stack[1] = 1;
            stack[1] = mem[stack[1]];
            stack[2] = 0;
            stack[1] = stack[2] == stack[1];
            if (! stack[1])
              {
                stack[1] = 2;
                stack[2] = 2;
                mem[stack[2]] = stack[1];
                _Ginterpret_1_0_15o/*1*/();
              }
          }
        else
          {
            stack[1] = 2;
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_30o/*2*/();
          }
      }
    else
      {
        stack[1] = 0;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[2] + stack[1];
        stack[2] = 0;
        mem[stack[2]] = stack[1];
        stack[1] = 3;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 3;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_30o/*2*/();
      }
    return;
  }


/* JMP 14 */
extern void _Ginterpret_1_0_15o/*79*/()
  {
    stack[1] = 2;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (stack[1])
      {
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 1;
        mem[stack[2]] = stack[1];
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (! stack[1])
          {
            stack[1] = 2;
            stack[2] = 2;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_15o/*1*/();
          }
      }
    else
      {
        stack[1] = 2;
        stack[2] = 3;
        mem[stack[2]] = stack[1];
        stack[1] = 3;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])
          {
            stack[1] = 2;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 2;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_15o/*1*/();
          }
        else
          {
            stack[1] = 0;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[2] + stack[1];
            stack[2] = 0;
            mem[stack[2]] = stack[1];
            stack[1] = 3;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_30o/*2*/();
          }
      }
    return;
  }

/* JMP 6 */
extern void _Ginterpret_1_0_6o()
  {
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (! stack[1])
      {
        stack[1] = 2;
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])
          {
            stack[1] = 1;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 1;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_6o/*95*/();
          }
        else
          {
            stack[1] = 2;
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            stack[1] = 3;
            stack[1] = mem[stack[1]];
            stack[2] = 0;
            stack[1] = stack[2] == stack[1];
            if (stack[1])
              {
                stack[1] = 2;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[1] - stack[2];
                stack[2] = 2;
                mem[stack[2]] = stack[1];
                _Ginterpret_1_0_15o/*1*/();
              }
            else
              {
                stack[1] = 0;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[2] + stack[1];
                stack[2] = 0;
                mem[stack[2]] = stack[1];
                stack[1] = 3;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[1] - stack[2];
                stack[2] = 3;
                mem[stack[2]] = stack[1];
                _Ginterpret_1_0_30o/*2*/();
              }
          }
      }
    return;
  }

/*
 * each load, add, etc. introduces a few assignments
 * each JEQ introduces a conditional
 *  - if (! ...) ... means fall-through
 * each JMP introduces a function call
 *
 */
extern void interpret_opt_cts_3/*96*/()
  {
    stack[1] = 0;		/* start */
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 2;
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 1;               /* 6 */  
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (! stack[1])		/* JEQ 54, no JMP */
      {
        stack[1] = 2;
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        stack[1] = 2;		/* 14 */  
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])		/* JEQ 47, yes JMP */
          {
            stack[1] = 1;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 1;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_6o(); /* JMP 6 */
          }
        else
          {
            stack[1] = 2;
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            stack[1] = 3;	/* 22 */
            stack[1] = mem[stack[1]];
            stack[2] = 0;
            stack[1] = stack[2] == stack[1];
            if (stack[1])	/* JEQ 40, yes JMP */
              {
                stack[1] = 2;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[1] - stack[2];
                stack[2] = 2;
                mem[stack[2]] = stack[1];
                _Ginterpret_1_0_15o(); /* JMP 14 */
              }
            else
              {
                stack[1] = 0;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[2] + stack[1];
                stack[2] = 0;
                mem[stack[2]] = stack[1];
                stack[1] = 3;
                stack[1] = mem[stack[1]];
                stack[2] = 1;
                stack[1] = stack[1] - stack[2];
                stack[2] = 3;
                mem[stack[2]] = stack[1];
                _Ginterpret_1_0_30o(); /* JMP 22 */
              }
          }
      }
    return;
  }
