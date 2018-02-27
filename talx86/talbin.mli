(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbin.mli 
 *
 * Defines annotations for the binary representation of tal files and
 * operations for splitting out and merging annotations.  
 *)
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;

type inst_annot = 
    An_none of int (* no annotation for next i instrs.  1 <= i <= 255 *)
  | An_op1 of coercion list 
  | An_op2 of coercion list * coercion list
  | An_op3 of coercion list * coercion list * coercion list
  | An_op4 of coercion list * coercion list * coercion list * coercion list
  | An_jcc of coercion list * inst_annot list
  | An_coerce of genop * coercion list
  | An_coercename of identifier * coercion list
  | An_fallthru of con list
  | An_malloc of identifier * (mallocarg option)
  | An_proof of (identifier * con list) list
  | An_unpacknomove of identifier * reg * coercion list
  | An_unpackmove of identifier * coercion list * coercion list
  | An_sunpack of identifier * genop
  | An_nameobj of identifier * genop
  | An_forgetunique of identifier
  | An_removename of identifier
(* LX *)
  | An_vcase of int32 * con * identifier * genop coerce 
  | An_letprod of identifier list * con
  | An_letroll of identifier * con 
(* end LX *)
(* Cyclone *)
  | An_cgstart of identifier * con
  | An_cgdump of identifier * identifier
  | An_cghole of identifier * identifier
  | An_cgholejmp of identifier * identifier coerce
  | An_cgholejcc of identifier * identifier coerce * inst_annot list
  | An_cgfill of identifier * identifier
  | An_cgfilljmp of identifier * identifier * identifier * identifier
  | An_cgfilljcc of identifier * identifier * identifier * identifier
  | An_cgforget of identifier * identifier
  | An_cgend
(* end Cyclone *)

type arep_item = An_con | An_kind | An_label
type data_annot = 
    An_dlabel of coercion list
  | An_dbytes of int     (* number of bytes *)
  | An_d2bytes
  | An_d4bytes of coercion list
  | An_dfloat32
  | An_dfloat64
  | An_djunk
  | An_dup
  | An_ddown
  | An_drep of arep_item

exception Found_Hole of (inst_annot list * Tal.instruction list)

(* Could a label potentially reside at the same location as another. *)
val virtual_label_map : code_block vector list -> (identifier -> bool)

(* inst_annot i f yields the annotation for instruction i, where
 * f is a partial function mapping labels to labels.  The intended
 * use of f is to map physical labels back to virtual labels in the
 * case that a code block is actually "virtual"  (i.e., has at most
 * virtual instructions in it.)  The issue is that such labels will
 * be assigned the same physical address as perhaps other labels and
 * thus when disassembling, we need to know that a particular operand
 * doesn't refer to the physical label but rather the virtual label.
 * If identifier->bool returns true then the label might coincide with
 * other labels.
 *)
val inst_annot : (identifier -> bool) -> instruction -> inst_annot
val merge_inst_annots :
    Disobjfile.objfile ->
      inst_annot list -> instruction list -> instruction list
val data_annots : data_item list -> data_annot list
