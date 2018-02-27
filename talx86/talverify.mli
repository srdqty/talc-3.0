(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker,                       *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talverify.mli
 * TAL Verifier
 *
 * Checks operands, instructions, interfaces, and implementations for well
 * formedness.
 *)

open Utilities;;
open Identifier;;
open Tal;;
open Talctxt;;

(* Coercions *)
val coercion_con : ctxt -> coercion -> con -> (con -> con) -> con
val coerce_con : (ctxt -> 'a -> con) -> ctxt -> 'a * coercion list -> con

(* Operands *)
val current_stack_con : ctxt -> con
val valid_stack_con : ctxt -> con -> con -> con
val coerce_reg_con : ctxt -> reg coerce -> con
val coerce_label_con : ctxt -> mode -> identifier coerce -> con
val genop_con : bool -> ctxt -> genop -> con
val coerce_genop_con : bool -> ctxt -> genop coerce -> con
val genop_write_at : ctxt -> genop -> con -> ctxt
val valid_binops : ctxt -> genop -> genop -> unit
val valid_cbinops : ctxt -> genop -> genop coerce -> unit

(* Instructions & Code Blocks *)
exception Terminal_Jmp
exception Fall_Thru of ctxt * con list
(* verify_instr add_block ctxt instr 
 *   Returns the context ctxt' that is the strongest post-condition
 *   of the instruction instr under the pre-condition context ctxt.
 *   The function add_block is called with the appropriate context
 *   and label, if the instruction jumps to a label that does not
 *   have a type.  This can be used to manage a work-list of blocks
 *   that need to be verified later (see verify_code_tree).  If the
 *   instruction is a terminal jump then there is no resulting post-
 *   condition, so the exception Terminal_Jmp is raised.  Similarly,
 *   if the instruction is a "Fallthru" to the next block, the 
 *   exception Fall_Thru is raised with the current context and
 *   the instantiation of the next block's constructor variables.
 *)
val verify_instr : (ctxt -> identifier -> unit) -> ctxt -> instruction -> ctxt
val add_code_labels : ctxt -> code_block vector -> mode -> ctxt
(* verify_code_tree label_map cbv ctxt i
 *   Verifies the tree of code blocks starting at index i.  The label at
 *   index i must have a type on it.  All blocks without a label that are
 *   reached by forward branches are also verified.  The label_map is used
 *   to find code blocks in cbv and to verify that branches to untyped
 *   labels are forward branches only.
 *)  
(* FMS: This isn't used outside either. 
val verify_code_tree :
    (identifier,int) Dict.dict -> code_block vector -> ctxt -> int
(* Cyclone *)
      -> (con option) option
(* End Cyclone *)
        -> unit
*)
(* FMS: This isn't used and its type changed so I am removing it from the interface. val verify_code_blocks : ctxt -> code_block vector
(* Cyclone *)
      -> (con option) option
(* End Cyclone *)
        -> unit
*)
val add_data_labels : ctxt -> data_block vector -> ctxt
val verify_data_block : ctxt -> tal_int_type -> data_block ->  con
val verify_data_blocks : ctxt -> tal_int_type -> data_block vector -> unit

(* Compilation Units *)
type ref2int = int_ref -> tal_int
(* Routines used by tallinkchk *)
val add_all_con_labels : ctxt -> tal_int array -> tal_int array -> ctxt
val verify_imports_exports : ctxt -> tal_int array -> ctxt
val get_int_type : tal_int array -> tal_int_type

(* init_ctxt ctxt0 tal_mod :
 * Given an initial context ctxt0 and a tal module, creates the context to
 * use when checking the implementation and the context to use when checking 
 * the export interface. 
 * NOTE: Verifies that the import and export interfaces are wll-formed. *) 
val init_ctxt : ctxt -> tal_mod -> (ctxt * ctxt)

(* verify_exports ctxte ctxti int :
   Check that the export interface (int) agrees with the implementation 
   context (ctxti).  
  *)
val verify_exports : ctxt -> ctxt -> tal_int vector -> unit 

(* verify_imp ctxt (imps_int_type,exps_int_type) imp:
 *   verifies that the implementation imp is correct starting in context ctxt.
 *   Uses the interface types to make sure implementation matches the interface.
 *)
val verify_imp : ctxt -> (tal_int_type * tal_int_type) -> tal_imp -> ctxt

(* From tallinkchk *)
val get_program_interface : ('a -> tal_int) -> 'a array -> 'a array -> 
   tal_int_type * tal_int_type


(* EOF: talverify.mli *)
