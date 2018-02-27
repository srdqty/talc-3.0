#ifndef __SIMP_H
#define __SIMP_H

// Failure macros.
// Use FAIL for errors that occur in the actual machine
// and BUG to indicate BUGs in the simulator.
// Both macros terminate the simulation.
#define FAIL(X) {fprintf(tal_stderr,"Fail: %s\n",X); \
                 raise(^Simp::Failure(X)); }
#define BUG(X) {fprintf(tal_stderr,"Bug: %s\n",X);  \
                 raise(^Simp::Failure(X));}

#define UNIMPLEMENTED { fprintf(tal_stderr,"Unimplemented\n"); \
                        raise(^Simp::Failure("Unimplemented")); }

#include "core.h"

extern exception Simp::Failure(string);
extern exception Simp::Exit(int);  // Code returned on exit.

extern FILE Simp::log_file;

extern string Simp::prog_name;

#endif // EOF: simp.h
