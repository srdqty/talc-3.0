#ifndef __MEM_H
#define __MEM_H

// Memory used by the simulator.  For now these are all functions,
// later we will likely have to macroize for performance.
#include "core.h"
#include "spec.h"

prefix Mem {
open Mem;

#define MEM_BLOCK_SIZE 0x4000
#define MEM_TABLE_SIZE 0x8000

#define MEM_BLOCK(A)  (((:ptr)(A) >> 16) & 0x7fff)
#define MEM_OFFSET(A)  (((A) & 0xffff) >> 2)

extern word mem_table[|MEM_TABLE_SIZE|]?[|MEM_BLOCK_SIZE|];
extern word mem_break_point; // Initialized by loader.
 extern ptr text_start; 
 extern ptr text_end;

// Makes sure address a is allocated. Raise an exception if not.
extern void tickle(ptr a);
extern bool is_allocated(ptr a);

// Read and write operations
// These operations assume the address is allocated and appropriately
// aligned.  That is word reads should be aligned on word boundaries.
// If the alignment is wrong they will fail SILENTLY.
extern byte  r_byte(ptr a);
extern short r_short(ptr a);
extern word  r_word(ptr a);
extern void  w_byte(ptr a, byte b);
extern void  w_short(ptr a, short s);
extern void  w_word(ptr a, word w);

// Checks that the address is valid to read nbytes from.  Checks alignment.
extern void r_valid(ptr a,int nbytes);
extern void w_valid(ptr a,int nbytes);

 extern void allocate_block(ptr a);
 // Unlike allocate_block, allocate_range does nothing if already allocated.
 extern void allocate_range(ptr lo, ptr hi); // inclusive.

// Copying values into and out of the simulator.

// Null terminates the string copied in.
extern void strcpy_in(ptr a, string s);
// Reads out a null terminated string and puts it in str. Include the null
// terminator in str, but may stop if no terminator found before end.
// Returns number of characters read including the terminator.
extern int strcpy_out(ptr a, string str);

extern void bcopy_in(ptr a, string x, int nbytes);
extern void wcopy_in(ptr a, word x[], int nwords);

// Copies up to nwords or until x is full words out of the simulators memory 
// starting at address a
 extern void wcopy_out(ptr a, word x[], int nwords);
 extern void bcopy_out(ptr a, string x, int nbytes);

// Initialize the memory subsystem.
extern void init();

// Dump len words of memory starting at a to file f
// The lower 2-bits of a shall be ignored.
extern void dump(FILE f, ptr a, int len);

// Print the ranges of allocated addresses.
extern void stats(FILE f);

}
#endif
