/* TEMPO Version X, 11/26/97, Copyright (c) IRISA/INRIA-Universite de Rennes */

#include "opcodes.h"

struct inst {
  int opcode;
  int val;
};

extern int mem[];
extern int stack[];
extern int prg_size;

void c_interpret(struct inst prg [])
{
  int pc = 0;
  int sp = 0;
  
  for( ; pc < prg_size; )
    if (prg[pc].opcode == PUSH)
      {
	sp = sp + 1;
	stack[sp] = (prg[pc]).val;
	pc = pc + 1;
      }
    else
      if (prg[pc].opcode == POP) {
	sp = sp - 1;
	pc = pc + 1;
      }
      else
	if (prg[pc].opcode == ADD)
	  {
	    stack[sp - 1] = stack[sp] + stack[sp - 1];
	    sp = sp - 1;
	    pc = pc + 1;
	  } 
	else
	  if (prg[pc].opcode == SUB)
	    {
	      stack[sp - 1] = stack[sp - 1] - stack[sp];
	      sp = sp - 1;
	      pc = pc + 1;
	    }
	  else
	    if (prg[pc].opcode == MUL)
	      {
		stack[sp - 1] = stack[sp] * stack[sp - 1];
		sp = sp - 1;
		pc = pc + 1;
	      }
	    else
	      if (prg[pc].opcode == LD)
		{
		  stack[sp] = mem[stack[sp]];
		  pc = pc + 1;
		}
	      else
		if (prg[pc].opcode == ST)
		  {
		    mem[stack[sp]] = stack[sp - 1];
		    sp = sp - 2;
		    pc = pc + 1;
		  }
		else
		  if (prg[pc].opcode == JMP)
		    {
		      pc = prg[pc].val;
		    }
		  else
		    if (prg[pc].opcode == JEQ) {
		      if(stack[sp] == 1)
			pc = prg[pc].val;
		      else
			pc = pc + 1;
		      sp = sp - 1;
		    }
		    else
		      if (prg[pc].opcode == CMP)
			{
			  stack[sp - 1] = (stack[sp] == stack[sp-1]);
			  sp = sp - 1;
			  pc = pc + 1;
			}
		      else
			{
			  printf( "\n Unrecognized instr: opcode-> %d  pc-> %d",
				  prg[pc].opcode, pc);
			}
}
