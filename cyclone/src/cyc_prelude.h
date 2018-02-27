
// For compiling Cyclone to gcc, every file must include this one.

// This file uses gcc extensions to C.
#ifndef CYC_PRELUDE_H
#define CYC_PRELUDE_H

#include <setjmp.h>

void *GC_malloc(int size);
void *GC_malloc_atomic(int size);

typedef double * Double;
typedef float  * Float;

// User code manipulates this for exception handling
extern jmp_buf curr_exn_handler;
typedef struct {int * tag; void * pkt;} exn_t;
extern  exn_t curr_exn;

// Note how we do types that are transparent and globally equivalent
// in Cyclone even though C is equivalent by name and we want separate
// compilation.  These two types are for std args.
#ifndef CYC_ARRCHAR
#define CYC_ARRCHAR
typedef struct {int sz; char *arr;} arr_Char;
#endif

#ifndef CYC_ARR_ARRCHAR
#define CYC_ARR_ARRCHAR
typedef struct {int sz; arr_Char *arr;} arr_arrChar;
#endif

// Caller casts to correct type and initializes elements!
// (Could do latter in callee with a default elt and a bcopy)
void * new_tagged_array(unsigned elt_sz, unsigned num_elts);

// This does the longjmp, so it ain't coming back
void cyc_raise(int * tag, void * pkt) __attribute__ ((noreturn));

#endif
