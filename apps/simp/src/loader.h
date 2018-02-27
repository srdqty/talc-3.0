#ifndef __LOADER_H
#define __LOADER_H

prefix Loader {
open Loader;

  extern struct initial_state {
    ptr initial_pc, initial_sp;
    ptr stack_base,data_base,text_base;
    word stack_size,data_size,text_size; // In bytes.
  }

  // Load an ecoff executable into the simulator.  Re-initializes the
  // simulator's memory too.
extern initial_state load(string args[], string envp[]);
}

#endif
