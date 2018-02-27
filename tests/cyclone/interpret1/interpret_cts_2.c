/* TEMPO Version 1.194, 04/27/98, Copyright (c) IRISA/INRIA-Universite de Rennes */


/* 
call signature of entry point interpret:
binding times of read non_locals: inst.val S, mem D, stack D, inst.opcode S, 
  prg_size S 
 */
struct inst { int opcode;
              int val; };
extern int mem[];
extern int stack[];
extern int printf();
extern int exit();
extern void _Ginterpret_1_0_0();


extern void interpret_cts_2/*0*/()
/* 
binding times of written non_locals: stack D, mem D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size, stack 
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

