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

void interpret_cts_4();

/* JMP 48 */
extern void _Ginterpret_1_0_22c/*88*/()
/* 
binding times of written non_locals: mem D, stack D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size 
residual non-locals bottom: stack 
evaluation time of body: D 
 */
  {
    stack[1] = 3;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (! stack[1])
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
        _Ginterpret_1_0_22c/*1*/();
      }
    return;
  }

/* JMP 27 */
extern void _Ginterpret_1_0_14c/*97*/()
/* 
binding times of written non_locals: mem D, stack D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size 
residual non-locals bottom: stack 
evaluation time of body: D 
 */
  {
    stack[1] = 2;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (stack[1])
      {
        stack[1] = 5;
        stack[2] = 3;
        mem[stack[2]] = stack[1];
        stack[1] = 3;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (! stack[1])
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
            _Ginterpret_1_0_22c/*1*/();
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
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_14c/*2*/();
      }
    return;
  }

/* JMP 6 */
extern void _Ginterpret_1_0_6c/*106*/()
/* 
binding times of written non_locals: mem D, stack D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size 
residual non-locals bottom: stack 
evaluation time of body: D 
 */
  {
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (stack[1])
      {
        stack[1] = 5;
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])
          {
            stack[1] = 5;
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            stack[1] = 3;
            stack[1] = mem[stack[1]];
            stack[2] = 0;
            stack[1] = stack[2] == stack[1];
            if (! stack[1])
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
                _Ginterpret_1_0_22c/*1*/();
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
            stack[1] = 2;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 2;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_14c/*2*/();
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
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 1;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_6c/*107*/();
      }
    return;
  }

extern void interpret_opt_cts_4()
/* 
binding times of written non_locals: mem D, stack D 
residual non-locals top: mem, stack 
eval non-locals top: inst.opcode, inst.val, prg_size 
residual non-locals bottom: stack 
evaluation time of body: D 
 */
  {
    stack[1] = 0;
    stack[2] = 0;
    mem[stack[2]] = stack[1];
    stack[1] = 5;
    stack[2] = 1;
    mem[stack[2]] = stack[1];
    stack[1] = 1;
    stack[1] = mem[stack[1]];
    stack[2] = 0;
    stack[1] = stack[2] == stack[1];
    if (stack[1])		/* JEQ 24 */
      {
        stack[1] = 5;		/* 24 */
        stack[2] = 2;
        mem[stack[2]] = stack[1];
        stack[1] = 2;
        stack[1] = mem[stack[1]];
        stack[2] = 0;
        stack[1] = stack[2] == stack[1];
        if (stack[1])		/* JEQ 45 */
          {
            stack[1] = 5;	/* 45 */
            stack[2] = 3;
            mem[stack[2]] = stack[1];
            stack[1] = 3;
            stack[1] = mem[stack[1]];
            stack[2] = 0;
            stack[1] = stack[2] == stack[1];
            if (! stack[1])	/* JEQ 66 */
              {
                stack[1] = 0;	/* fall-through */
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
                _Ginterpret_1_0_22c(); /* JMP 48 */
              }
          }
        else			/* JEQ 45 fall-thru */
          {
            stack[1] = 0;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[2] + stack[1];
            stack[2] = 0;
            mem[stack[2]] = stack[1];
            stack[1] = 2;
            stack[1] = mem[stack[1]];
            stack[2] = 1;
            stack[1] = stack[1] - stack[2];
            stack[2] = 2;
            mem[stack[2]] = stack[1];
            _Ginterpret_1_0_14c(); /* JMP 27 */
          }
      }
    else			/* JEQ 24 fall-thru */
      {
        stack[1] = 0;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[2] + stack[1];
        stack[2] = 0;
        mem[stack[2]] = stack[1];
        stack[1] = 1;
        stack[1] = mem[stack[1]];
        stack[2] = 1;
        stack[1] = stack[1] - stack[2];
        stack[2] = 1;
        mem[stack[2]] = stack[1];
        _Ginterpret_1_0_6c();	/* JMP 6 */
      }
    return;
  }

