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
extern int printf();
extern int exit();
void interpret_cts_2();


void interpret_opt_cts_2()/*0*/
/* 
binding times of written non_locals: mem D, stack D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size 
residual non-locals bottom: stack 
evaluation time of body: D 
 */
  {
    stack[1] = 1234;
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 5678;
    stack[1] = stack[2] + stack[1];
    stack[2] = 4;
    stack[1] = stack[2] * stack[1];
    stack[2] = 3;
    stack[1] = stack[2] * stack[1];
    stack[2] = 21;
    stack[1] = stack[2] * stack[1];
    stack[2] = 1740590;
    stack[1] = stack[1] - stack[2];
    stack[2] = 3;
    mem[stack[2]] = stack[1];
    return;
  }

