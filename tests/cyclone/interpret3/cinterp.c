#include "opcodes.h"

struct inst
{
  int opcode;
  int val;
};

extern int mem[];
extern int stack[];
extern int prg_size;

void c_interpret(struct inst prg[], int pc, int sp)
{ 
  /*
  printf("pc/prg=%d/%d INSTR={%d, %d} mem[0]=%d, mem[1]=%d\n", 
	 pc, prg_size, prg[pc].opcode, prg[pc].val, mem[0], mem[1]);
  */
  if (pc < prg_size)
    {
      if(prg[pc].opcode == PUSH)
	{
	    sp = sp + 1;
	    stack[sp] = prg[pc].val;
	    pc = pc + 1;
	    c_interpret(prg, pc, sp);
	}
      else
	if(prg[pc].opcode == POP)
	  {
	    sp = sp - 1;
	    pc = pc + 1;
	    c_interpret(prg, pc, sp);
	  }
	else
	  if(prg[pc].opcode == ADD)
	    {
	      stack[sp-1] = stack[sp] + stack[sp-1];
	      sp = sp - 1;
	      pc = pc + 1;
              c_interpret(prg, pc, sp);
	    }
	  else
	    if(prg[pc].opcode == SUB)
	      {
		stack[sp-1] = stack[sp-1] - stack[sp];
		sp = sp - 1;
		pc = pc + 1;
	        c_interpret(prg, pc, sp);
	      }
	    else
	      if(prg[pc].opcode == MUL)
		{
		  stack[sp-1] = stack[sp] * stack[sp-1];
		  sp = sp - 1;
		  pc = pc + 1;
	          c_interpret(prg, pc, sp);
		}
	      else
		if(prg[pc].opcode == LD)
		  {
		    stack[sp] = mem[stack[sp]];
		    pc = pc + 1;
	            c_interpret(prg, pc, sp);
		  }
		else
		  if(prg[pc].opcode == ST)
		    {
		      mem[stack[sp]] = stack[sp-1];
		      sp = sp - 2;
		      pc = pc + 1;
	              c_interpret(prg, pc, sp);
		    }
		  else
		    if(prg[pc].opcode == JMP)
		      {
			pc = prg[pc].val;
	                c_interpret(prg, pc, sp);
		      }
		    else
		      if(prg[pc].opcode == JEQ)
			{
			  if(stack[sp])
			    { pc = prg[pc].val; sp = sp - 1 ; c_interpret(prg,pc, sp); } 
			  else
			    { pc = pc + 1; sp = sp - 1; c_interpret(prg, pc, sp); } 
			}
		      else
			if(prg[pc].opcode == CMP)
			  {
			    stack[sp-1] = (stack[sp] == stack[sp-1]);
			    sp = sp - 1;
			    pc = pc + 1;
	                    c_interpret(prg, pc, sp);
			  }
			else
			  {
			    printf("\n Unrecognized instruction -> %d at %d\n",
				   prg[pc].opcode, pc);
			    exit();
			  }
    }
}
