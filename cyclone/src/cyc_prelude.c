
// For compiling Cyclone to gcc, we always link in this file.
// BDW collector must be linked in too of course.
// Furthermore, every file must also include prelude.h

// This file uses gcc extensions to C.

#include "cyc_prelude.h"
#include <stdlib.h>
#include <stdio.h>

// This will work until we run out of memory. :-)  Need to build BDW!!!

// Note that since U is prepended to user code, our main calls Umain.
// Command-line args are passed in a tagged array.
extern int Umain(arr_arrChar *);

jmp_buf curr_exn_handler;
exn_t   curr_exn;

// Caller casts to correct type and initializes elements!
void * new_tagged_array(unsigned elt_sz, unsigned num_elts) {
  struct ans_t {unsigned sz; void * arr;};
  struct ans_t *ans = (struct ans_t *)GC_malloc(sizeof(unsigned)+sizeof(void*));

  ans->sz  = num_elts;
  if(num_elts > 0) 
    ans->arr = (void *)GC_malloc(elt_sz*num_elts);
  return ans;
}

void cyc_raise(int * tag, void * pkt) {
  curr_exn.tag = tag;
  curr_exn.pkt = pkt;
  longjmp(curr_exn_handler,1);
}

int main(int argc, char **argv) {

  // set up command-line args
  arr_arrChar * args = (arr_arrChar *)new_tagged_array(sizeof(arr_Char), argc);
  arr_Char    * elts = args->arr;
  int           i;
  int           status;

  for(i=0; i < argc; ++i) {
    char *str = argv[i];
    elts[i].sz  = strlen(str); // "forget" the null-terminator??? (o.w. + 1)
    elts[i].arr = str;
  }

  // set top-level exception handler
  status = setjmp(curr_exn_handler);
  if(status) {
    // Later extend this to give better information.
    printf("\nUncaught exception caused immediate termination.\n");
    return status;
  }

  // rest is up to the Cyclone program
  return Umain(args);
}
