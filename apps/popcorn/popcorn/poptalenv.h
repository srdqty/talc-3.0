#ifndef POPTALENV_H
#define POPTALENV_H

#include "core.h"
#include "xarray.h"
#include "dict.h"
#include "id.h"
#include "set.h"
#include "bitvec.h"

#include "poperr.h"
#include "popil.h"
#include "tal.h"

prefix Poptalenv {
open   Poptalenv {

open Core {
open Popil {
open Tal {

#define list   List::list
#define id     Id::id
#define dict   Dict::dict
#define xarray Xarray::xarray

#define NUM_FP_REGS 7
#define NUM_REAL_FP_REGS 8
#define NUM_REGS 6

extern struct reg_assignment {
  // what the register allocator passes to the code generator
  tal_place     assignment[]; // the actual assignment to regs and slots
  <int>Set::set all_live[][]; // needed for TAL label types
  int           num_slots;    // number of stack slots used for spills
  int           double_slots; // number of doubleword slots
  int           names[];      // names[i] is TAL name to use for variable i
                              // (aliases must use same name)
}

// The stack is partitioned into regions described below.
// There are optionally two words at the very top of the stack for 
// the handler if there is a local handler. These are omitted here.
// 
// For each region we include the depth at which it starts in words,
// the number of words consumed by the region, and the number of
// stack slots consumed by this region (if any).  
//
// For alignment reasons there may be padding between regions.
extern struct stack_desc {
  int handler_start, handler_words;
  int slot_start, slot_words, slot_slots; // Slots shallower than callee-save
  int save_start, save_words; // Takes no stack slots. Includes return address
  int param_start, param_words, param_slots;
  
  int double_slots; // Duplicated from assignment info.
  int total_slots;

  con retargs; // Instantiation for return args.
  <con>Opt return_address_con; // Type of the return address on the stack.
}

// This environment is used to translate floating point code. 
 extern struct fp_env {
   // Below change per instruction
   // track usage of registers as we compile.  
   int alloc_to_real[NUM_REAL_FP_REGS]; // Mapping from alloc'd regs to real regs.
   int real_to_alloc[NUM_REAL_FP_REGS]; // Mapping from real FP regs to alloc'd regs.
   int st0; // Which real register is the top of the stack (st0).
   bool emitted;
 }

extern fp_env dummy_fp_env;

// This environment is for the term translation
extern struct cg_env {
  // later put stuff in here for type-sharing (or for prototype do as post pass)
  // probably need two-way map between cons and abbrevs
  
  // this stuff only changes once per function 
    // set by caller of trans_function
  cf_function    fn; 
  reg_assignment assignment_info;
    // set by trans_function
  stack_desc     stack_desc;

  <int,con>dict  handler_types; // block num to label type (handlers only)
  bool           fun_has_handler; // false if no handlers in fun
  BITVEC         label_types; // bitvec set if put type on label
  bool           ebx_used;
  bool           esi_used;
  bool           edi_used;
  int            num_callee_used;

  *(string,bool) tyvars[];
  BITVEC      local2tyvars[];
  BITVEC      tyvar_bvs[]; // One bitvector of tyvars per block.

  // FP
  int fps_live[][]; // Bitmask of occupied FP regs for each instruction.
  fp_env fp_envs[]; // Initial environment per block.

  // this stuff changes once per block
  int                 il_block; // implies current handler too
  <instruction>xarray current_instrs; // new one for each block

  // this stuff changes as we walk forward through a block
  int                      il_inst;
  <int, cf_refinement>dict refined_locals;
  <int, *(int[],int)>dict  partial_inits;
  <int>Opt          ebx_local; // unnecessary once we have a supertype
  <int>Opt          esi_local;
  <int>Opt          edi_local;
     // some day put array bounds stuff here
  <*(id,<con>Opt,<instruction>xarray)>xarray other_blocks; // for jump trees
     // may need some web stuff here to help operand2genop!

  // this stuff is what we're building (note that code_blocks are printed
  // as they're created for space considerations)
  <*(id,con)> xarray exports;

  // FP
  fp_env fp_env; 

   // Cyclone +

  <int, int[]>dict regions;  /* For each live region, a bitvector of
                              template pointers that might be the last
                              dumped at this point. */
  <int,<int>list>dict   filled; // For each template pointer, 
  // which holes are filled.
  cf_template current_template; // May be null

  Tal::con hole_cons[]; // Pre-condition for each hole.

  // information tracked across functions.
  <string,BITVEC>dict jcc_holes; // function name --> bitvector(1 = jcc,0 = jmp)

  <cf_template,Tal::con>dict template_cons; // TAL con for each template.
  <cf_template,cg_env>dict template_post_envs;
  // Cyclone -
} 

extern union tal_place {
  reg Reg;
  int Fpreg; // 0 through 7 only.
  int Stackslot;
}

extern bool in_handler     (cg_env env);
extern bool is_handler     (cg_env env, int blockNum);
extern bool handler_changes(cg_env env, int block_num);

 extern stack_desc new_stack_desc(); // Allocate a dummy stack description.
extern cg_env new_cg_env (cf_function fn, <*(id,con)>xarray exports);

extern cg_env copy_cg_env(cg_env env);

extern cg_env join_cg_env(cg_env e1, cg_env e2);

// environment used for type translation
extern struct tg_env {
  <*(id,kind,int_con_def)>xarray export_cons;
  <*(id,kind,int_con_def)>xarray import_cons;
  <*(id,kind,con)>        xarray file_cons;
  <*(id,con)>             xarray file_abbrevs;
}

// environment used for exception translation
extern struct dg_env {
  <data_block>xarray blocks; 
  <*(id,con)> xarray imports;
  <*(id,con)> xarray exports;
}

extern void print_stack_desc(FILE f, stack_desc sd);

#undef list
#undef dict
#undef xarray
#undef id

}}}}}
#endif
