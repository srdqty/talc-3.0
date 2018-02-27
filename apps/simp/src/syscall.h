#ifndef __SYSCALL_H
#define __SYSCALL_H

prefix Syscall {

  extern int call_count[]; // Counts number of calls to each system code.
  extern void init();  
  extern void call(word inst_a, word inst_b, Sim::register_file rf);
  extern void print_call_count(FILE f);

}
#endif
