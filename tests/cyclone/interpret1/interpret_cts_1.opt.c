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


void interpret_opt_cts_1/*0*/()
/* 
binding times of written non_locals: stack D, mem D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size, stack 
evaluation time of body: D 
 */
  {
    stack[1] = 0;
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 111;
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 222;
    stack[2] = 2;
    mem[stack[2]] = stack[1];
    stack[1] = 333;
    stack[2] = 3;
    mem[stack[2]] = stack[1];
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 2;
    stack[1] = mem[stack[1]];
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 3;
    stack[1] = mem[stack[1]];
    stack[2] = 2;
    mem[stack[2]] = stack[1];
    stack[1] = 0;
    stack[1] = mem[stack[1]];
    stack[2] = 3;
    mem[stack[2]] = stack[1];
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 2;
    stack[1] = mem[stack[1]];
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 3;
    stack[1] = mem[stack[1]];
    stack[2] = 2;
    mem[stack[2]] = stack[1];
    stack[1] = 0;
    stack[1] = mem[stack[1]];
    stack[2] = 3;
    mem[stack[2]] = stack[1];
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 2;
    stack[1] = mem[stack[1]];
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 3;
    stack[1] = mem[stack[1]];
    stack[2] = 2;
    mem[stack[2]] = stack[1];
    stack[1] = 0;
    stack[1] = mem[stack[1]];
    stack[2] = 3;
    mem[stack[2]] = stack[1];
    return;
  }
