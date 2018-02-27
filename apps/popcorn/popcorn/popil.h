
// A major change would be to abstract the interface so things like types
// can be shared.

#ifndef POPIL_H
#define POPIL_H

#include "core.h"
#include "list.h"
#include "set.h"
#include "dict.h"
#include "id.h"
#include "xarray.h" 
#include "bitvec.h"
#include "popsyntax.h"
#include "tal.h"

// FMS: XXX, need some temporary convenient place to stick these.
// Perhaps core.pop?
#define LOC(X)  sprintf(__FILE__ " %d: %s",__LINE__,X)
#define FAIL(X) raise Failure(LOC(X))
#define BUG(X)  { printf("%s\n",LOC(X)); raise Impossible(LOC(X)); }

prefix Popil {
open   Popil {

#define cf_var     string
#define cf_typname string
#define cf_hole    int
#define cf_local   int

#define list   List::list
#define dict   Dict::dict
// #define set    Set::set
#define Opt    Core::Opt
#define id     Id::id
#define xarray Xarray::xarray

////////////// File, function, block, declaration structures ///////////////
extern struct cf_file {
  <cf_typname, cf_absdecl>    dict abstracts;
  <cf_typname, cf_structdecl> dict structs;
  <cf_typname, cf_uniondecl>  dict unions;
  <cf_typname, cf_abstypedecl>dict abstypes;
  <id,         cf_exndecl>    dict exns; // must be id b/c an operand
  <id>Set::set                     void_pkts; // hack to apease operand_type
  <id,         cf_typ>        dict extern_vals;
  <id,         cf_globaldecl> dict globals;
  <id,         cf_function>   dict funs;
  <id,         cf_function>   dict rtcg_funs; // Made up of templates.
  bool has_main;
}
extern ?struct cf_function {
  cf_file              file;
  string               name;
  cf_convention        convention;
  bool                 is_static;
  <cf_var>list         tyvars;
  <cf_var>list         exist_tyvars; // may contain duplicates!
  <cf_var>list         sing_tyvars; // should not contain duplicates.
  <cf_typ>list         arg_typs;
  <cf_typ>Opt          ret_typ;
  <cf_idinfo>xarray    all_operands;  // keeps type info out of instructions
  <cf_constinfo>xarray all_consts;    // keeps type info out of instructions
  <int>list            params;
  cf_block             entry;
  <cf_block>xarray     blocks; // ASSUMED INVARIANT: blocks[i].num == i
  <int[]>Opt           df_order; // ASSUMED: correct if not null
  // by using an array, analyses can piggyback info by using parallel arrays

  // Cyclone +
  // About this function. 
  cf_function          generator; // WARNING: may be null
  <cf_template>list    templates; // Templates this function is made up of.
  <cf_holeinfo>xarray  all_holes; 

  //About generated functions.
  <*(<cf_local>xarray,cf_function)>xarray code_regions; // Internal xarray is a list of local operands that at some point contain template pointers.
  <cf_template>xarray template_pointers;
  // Cyclone -
}
extern struct cf_block {
  cf_function fn;    // pointer back to function-level data
  id          label; // name of this block -- makes output more readable
  int         num;   // unique to this block and index in fun's blocks array

  <int>Opt    handler_var; // null means not a handler

  <cf_instruction>xarray insts; // actual instructions
  cf_transfer            trans; // expression for leaving the block

  <cf_block>Opt    handler; // innermost local exn handler, if any
  <cf_block>xarray pred;    // incoming edges
  <cf_block>xarray succ;    // outgoing edges -- these might be implied

  <int, cf_refinement>dict refined_locals; // vars with refined types
  <*(cf_sing,cf_sing)>list ltes;          // constraints for bounds-elim???
  <int, *(int[],int)>dict  partial_inits; // partial initialization info

  // Cyclone +
  cf_template template; // Template this block occurs within.

  //On entry!
  <int,BITVEC>dict regions; /* For each region, stores a bitvector of
			      templates that might be last dumped at
			      this point. rgn is in scope if its in
			      the dictionary at all. */
  /* poptranslate computes the scope correctly, but not the bitvector.
     The bitvector is computed by popiltype.pop. */

  <int,<cf_hole>list>dict filled; // Could be an xarray instead of a list. 
  // Cyclone -

}

 extern struct cf_field {
   cf_typ typ;
   cf_capability cap;
   int offset; // In bytes
 }

extern struct cf_structdecl {
  Popsyntax::scope scope;
  cf_typname       name;
  <cf_var>list     tyvars;
  bool             possibly_null;
  //  *(cf_typ,cf_capability) fields[];
  cf_field         fields[];
  field_name       field_names[];
}
extern struct cf_uniondecl {
  Popsyntax::scope scope;
  cf_typname       name;
  <cf_var>list     tyvars;
  *(<int>list, <*(int,cf_typ)>list) fields;
}
extern struct cf_absdecl {
  <cf_var>list tyvars;
  bool         possibly_null;
}
extern struct cf_abstypedecl {
  Popsyntax::scope scope;
  cf_typname       name;
  <cf_var>list     all_tyvars;
  <cf_var>list     exist_tyvars;
  cf_typ           defn;
}
extern struct cf_exndecl {
  Popsyntax::scope scope;
  id               name;
  <cf_typ>Opt      typOpt;
}
extern struct cf_globaldecl {
  Popsyntax::scope   scope;
  cf_typ             typ;
  Tal::data_block    data; // if we ever need values for an analysis, this
       // will be a pain, but if not, it save us from defining more syntax
       // cannot leave in popsyntax form b/c of NewUnion and variant tags
}

// Cyclone +
// Shouldn't templates know what code region they are in?
// Otherwise how do we emit code.
extern ?struct cf_template {
  id               name;
  id               end_name;
  <cf_block>xarray blocks;
  cf_block         entry;
  <*(cf_block,cf_hole)>Opt    exit; // No exit block implies it has false 
  // post-condition.
  <*(cf_hole,cf_block)>xarray holes;  // Holes occuring within the template.
  <cf_block>Set::set labels; // Blocks targeted by fills.
}
// Cyclone -

//////////////////////// Terms and Such /////////////////////
extern union cf_instruction {
  void Nop;

  *(cf_operand, cf_operand)                                Copy;
  *(Popsyntax::primop, cf_operand)                         Nullop;
  *(Popsyntax::primop, cf_operand, cf_operand)             Unop; 
  *(Popsyntax::primop, cf_operand, cf_operand, cf_operand) Binop;

  *(cf_operand, cf_operand, cf_typedcopy) TypedCopy;// like a Copy, but gunky

  *(int, cf_operand, cf_operand) SelectField;
  *(int, cf_operand, cf_operand) AssignField;

  // Both arguments are singletons.  Adds o1 < o2 to the environment.
  // If check fails will exit program.
  // To keep basic blocks reasonably large we don't terminate blocks
  // with array checks.
  *(cf_operand, cf_operand) ArrayCheck;
  *(cf_operand,int)         CArrayCheck; // Compare a singleton to an int.
  // dest, unboxed array, singleton offset
  *(cf_operand, cf_operand, cf_operand) ArraySub;
  // unboxed array, singleton offset, value
  *(cf_operand, cf_operand, cf_operand) ArrayUpd;

  cf_operand New;

  *(cf_operand,cf_operand) Rdtsc; // Gets clock cycles since last boot.

  // Cyclone +
  // Figure out where to emit Forget instructions automatically.
  cf_operand                           Start; // Type of op. provides "name".
  *(cf_operand,cf_operand,cf_template) Dump;  // codeRgn,template ptr,template
  *(cf_hole,cf_operand)                Hole;  // Magic definition of operand.
  *(cf_operand,cf_operand,cf_hole,cf_operand) 
                                       Fill;  // codeRgn, templ ptr, hole, value
  *(cf_operand,cf_operand,cf_hole,cf_operand,cf_block)
				       FillJmp; // ..., templ_ptr of value,block
  *(cf_operand,cf_operand)             End;   // dest,codeRgn.
  //Cyclone -

}
extern  union cf_typedcopy {
  void         InitRoll; // Roll up a tuple type into the appropriate shape.
  <cf_typ>list Instantiation;
  <cf_var>list Unpacking; // Used for regular existentials, and singletons.
  <cf_typ>list Packing; // Can also be used to pack a singleton into an int.
}
extern union cf_transfer { 
  void           Abort;  // Cyclone: Halt the program
  cf_dest        Uncond; // may enter/leave a try, we can tell by handlers
  cf_operand      Raise; 
  <cf_operand>Opt Retn;  

  // result (if non-void), function val, type args, term args
  // note due to exceptions we may transfer to the handler from a Call
  *(<cf_operand>Opt, cf_operand, <cf_typ>list, <cf_operand>list,
    cf_dest) Call; 

  *(cf_operand, <cf_typ>list, <cf_operand>list) TailCall;
  
   // true block, false block, lhs, rhs, comparison
  *(cf_dest, cf_dest, 
    cf_operand, cf_operand, Popsyntax::primop)   Cond; 

  *(cf_operand, <*(int,cf_dest)>list, cf_dest) NumSwitch; 
  // exp, (tag,block)s (variant,block)s, default
  *(cf_operand, <*(int,cf_dest)>list, 
    <*(int,cf_dest)>list, <cf_dest>Opt)        SumSwitch; 
  // exp, (variant,lbl)s, default
  *(cf_operand, <*(id,cf_dest)>list, cf_dest)  ExnSwitch;  

  *(cf_operand, cf_dest) NullCheck;
           // int is variant, bool is void not value
  *(cf_operand, int, bool, cf_dest) UnionCheck; 
  *(cf_operand, cf_dest, cf_dest) NullBranch; //exp, notNull block, null block
}
extern union cf_operand {
  // ints are indices into appropriate xarrays (and used for bit vectors)
  int Const;  // xarray all_consts
  int Local;  // xarray all_locals
  id  Global; // id or string? some day make this an xarray too? maybe several?
}

// Cyclone +
// Destination for control transfers.  Used to be cf_block before RTCG.
extern union cf_dest {
  cf_block Known;
  cf_hole  Unknown;
}

extern union cf_holeinfo {
  cf_typ Value;
  <cf_block>list Jmp; // possible successors, no duplicates allowed!!
  <cf_template>list Terminal;
}
// Cyclone - 

////////////////////// Types and Such  ///////////////////////////

extern union cf_convention {
   void Stdcall, Cdecl;
}

extern struct cf_idinfo {
  cf_typ  typ;
  cf_use  usage;
}
extern struct cf_constinfo {
  int    val;
  cf_typ typ;
}
extern union cf_use { 
  void          Generated;
  string        FromAbove; // string for diagnostic purposes
  *(string,int) Param;     // eg. int f(int b, int c) has b as 1 and c as 2
}
extern union cf_typ { // later should make abstract and share them all
  void         Exn; 
  id           ExnName;  // much like a singleton, needed for partial init only
                         // (should probably go away)
  cf_var       Var;
  void         Bool;
  cf_numtype   Num;      // used for Char too
  void         Float;
  void         Double;
  *(cf_convention,
    <cf_var>list,<cf_typ>Opt,<cf_typ>list) Fn; // tyvars, retn, params

  // maybe unpacking should be implicit and size should be an identifier?
  // that makes sense since we don't (yet) optimize away size field, 
  // just the checks.
  *(<int>Opt,cf_typ) Array; // used for string too, int is size if known!
  *(cf_sing,cf_typ)  UnpackedArray; // size, elmt typ
  cf_sing            Sing;          

  *(cf_typname, <cf_typ>list)       Named; // list is instantiation
  //  *(cf_typ,cf_capability)           Tuple[]; 
  cf_field                          Tuple[];
  cf_typ                            Option; // Possibly null type.
  // Cyclone +
  int CodeRgn;   // int is position in code_regions within cf_function
  int TemplPtr;  //  "               " template_pointers "          "
  // Cyclone -

}
extern struct cf_numtype {
  Popsyntax::size_of s;
  bool is_signed;
}
extern union cf_capability {
  void Read, ReadWrite;
}
extern union cf_sing {
  cf_var Var;
  int    Int;
}
extern union cf_refinement {
  // we used to refine to another cf_typ, but these types are too high-level 
  // to encode what we want.  So we go to the other extreme: special-cases
  // for the various forms of refinement we actually do.

  // if we do array-bounds-elim, add appropriate variants for that here?
  // (that will gum up Popiltal even more, since that module currently
  //  assumes numeric types won't be refined).

  // Doing this could cause more substitutions on polymorphic types since
  // we're effectively delaying the substitution until the capability is
  // written down.  Memoizing would eliminate this shortcoming.
  void NotNull;
  int  VoidVariant;
  int  ValueVariant;
  id   ExceptionVariant;

  void IsNull; // these are used for unrefining
  void UnknownUnion;
  void UnknownException;
}

/////////////////////// Useful Things ////////////////////////////

// Return a list of all functions so that generated functions always
// precede their generators.
 extern <cf_function>list get_fun_order(cf_file f); //Cyclone


extern int get_df_order(cf_function f)[];
extern int reverse_topological_sort(cf_function f)[];

// Pull out all blocks related to template t from order,
// and make template entry and exit first and last respectively.
extern int template_partition(cf_function f, cf_template t, 
			      bool incr, int order[])[];

extern int    install_local(cf_function f, cf_typ t, string s);
extern int    install_param(cf_function f, cf_typ t, string s, int pos);

// Cyclone +
extern int install_hole(cf_function f, cf_holeinfo hi);
extern cf_holeinfo hole_info(cf_function f,cf_hole h);

// Cyclone -
extern int    newtemp      (cf_function f, cf_typ t);
extern int    newconst     (cf_function f, int v, cf_typ t);

extern cf_var newtyvar     (); //return a brand-new type variable.

// WARNING: hole names must be globally unique, although verifier does not
// check this. Assembler crashes if this does not hold.
extern id hole2id(cf_template, cf_hole); // Generate a name for a hole.

extern bool is_nullable(cf_file f, cf_typ t);
extern cf_typ operand_type (cf_function f, cf_operand op);

extern cf_operand localoperand (int i);
extern cf_operand constoperand (int i);

// the following should only be called on two blocks in the same function!
extern int        cf_block_cmp (cf_block, cf_block);
extern void       add_edge(cf_block src, cf_block dest);

// Compare two templates by their names.
extern int cf_template_cmp(cf_template,cf_template);/*  */

// should only be called on two operands in the same function
extern int cf_operand_cmp(cf_operand op1, cf_operand op2);

// useful for things like nonnull_typ
extern cf_typ typ_subst(<*(cf_var,cf_typ)>list, cf_typ);

// in bytes!
extern int sizeof_typ(cf_typ t);

extern cf_typ u_int,s_int,u_char;

extern bool is_fp_op(cf_function,cf_operand);

// Primop has at least one floating point argument
 extern bool is_fp_primop(Popsyntax::primop);
 // Primop is a floating point relational operator.
 extern bool is_fp_relop(Popsyntax::primop);
 // Primop has one floating point argument but at least one non-floating point.
 extern bool is_partial_fp_primop(Popsyntax::primop);
 // Instruction has at least one floating point operand
 extern bool is_restricted_fp_binop(Popsyntax::primop);
 extern bool is_fp_inst(cf_function,cf_instruction);
 // Instruction has at least one floating point operand, and at least one 
 // non-floating point.
 extern bool is_partial_fp_inst(cf_function,cf_instruction);

#undef cf_var    
#undef cf_typname

#undef list  
#undef dict  
// #undef set
#undef Opt   
#undef bitvec
#undef id
#undef xarray

}}

#endif
