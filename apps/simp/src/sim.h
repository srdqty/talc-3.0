#ifndef __SIM_H
#define __SIM_H

#include "spec.h"
#include "core.h"

prefix Sim {
open Sim;

extern struct register_file {
  word gp[|32|]; // 32 of these
  word fp[|32|]; // 32 of these
  word hi,lo;
  word fcc,tmp,mem,ctrl;
  word pc;
}

 extern unsigned int instructions_executed;

extern register_file new_register_file();
extern void          execute_n(register_file rf,unsigned int max_insts);

// Print instruction passed in.  
// Instruction is assumed to reside at pc for purposes of computing addresses
// of pc-relative jump targets.
extern void print_insn(FILE f,word inst_a, word inst_b,
		       register_file rf,
		       ptr pc);

 extern void verbose_print_insn(FILE f,word inst_a, word inst_b,
				register_file rf,
				ptr pc);

extern void print_register_file(FILE f, register_file rf);
}

#endif


