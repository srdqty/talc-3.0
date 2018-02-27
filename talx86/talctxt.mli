(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talctxt.mli
 * Contexts for verification of TAL kinds, type constructors, and terms.
 *
 *)

open Numtypes;;
open Identifier;;
open Tal;;

(*** Locations ***)

type tal_loc =
    Loctop 
  | Lockind of identifier   (* kind abbrev  - LX *)
  | Loccon of identifier   (* Imported/Exported type label *)
  | Locval of identifier   (* Imported/Exported value label *)
  | Loccb of identifier    (* Con block l *)
  | Locc of identifier*int (* Code block l instruction i *)
  | Locd of identifier*int (* Data block l item i *)
;;

(*** Verification Errors *)

type verify_error =
    Undefined_label of identifier
  | Undefined_var of identifier
  | Undefined_reg of reg
  | Redefined_label of identifier
  | Kindleq of kind*kind
  | Kindeq of kind*kind
  | Kindmeet of kind*kind
  | Kindjoin of kind*kind
  | Conwf of con*string
  | Neqcon of con*con
  | Nleqcon of con*con
  | Msabsentreg of reg*machine_state*machine_state
  | Msnleq of reg*con*con
  | Conmeet of con*con
  | Conjoin of con*con
  | Msmeet of machine_state*machine_state
  | Msjoin of machine_state*machine_state
  | FPstackeq of fpstack*fpstack
  | FPstackleq of fpstack*fpstack
  | BadUnroll of con  (* con is not unrollable *)
  | Unknown_size of con
  | Bad_offset of int32 (* -n means n bytes before a field/slot
                           +n means n bytes after last valid field/slot *)
  | Bad_depth of int
  | Not_tail of con*con
  | Readonly
  | Stack_write_alignment
  | Coercion of con*string*coercion
  | No_stack_type
  | Genop of string*genop
  | Both_mem of genop*genop
  | Inst_form of string
  | Data_form of string
  | Fallsthru
  | Cyclic_imports of int_ref
  | Doesnt_export
  | Ndisjoint_exports of identifier
  | Multiple_exports
  | Con_def_nleq
  | Intt_nleq of string * identifier
  | Label_requires_type of identifier
  | Label_bad_mode of identifier * mode
  | Fallsthru_unlabelled
  | Backward_branch of identifier
  | Bad_Name of identifier
  | Var_defined of identifier
(* ---- LX ---- *)
  | Negative_var of identifier
  | Kindwf of kind * string
(* -- end LX -- *)
;;

exception Talfail;;   (* The verifier could not recover from errors *)

(*** Contexts ***)

type ctxt;;
exception Talverify of ctxt * verify_error;;

val empty_ctxt : ctxt
(* ctxt_join c1 c2:  adds definitions in c2 to c1. *)
(* NB:  these really only make a join when we're not processing anything
 * but top-level definitions. I need them in the verifier only to glue
 * together the cheap, vheap, etc. from imports.  In particular, the
 * machine states are not joined, the prop is lost, etc.  So use with
 * caution. 
 *)
val ctxt_join : ctxt -> ctxt -> ctxt
val get_loc : ctxt -> tal_loc
val get_verify_ctxt : ctxt -> string
val generate_error : ctxt -> verify_error -> unit

val get_label_kind : ctxt -> identifier -> kind
val get_variable_kind : ctxt -> identifier -> kind
val get_abbrevs : ctxt -> (identifier,con) Dict.dict
val get_locals : ctxt -> (identifier,con) Dict.dict
val get_label_def : ctxt -> identifier -> int_con_def
val get_label_con : ctxt -> identifier -> con
val get_label_con_opt : ctxt -> identifier -> con option
val get_label_con_mode : ctxt -> identifier -> (con * mode)
val get_label_con_opt_mode : ctxt -> identifier -> (con option * mode)
val get_value_labels : ctxt -> (identifier,con option * mode) Dict.dict
val get_reg_con : ctxt -> reg -> con
val get_machine_state : ctxt -> machine_state
val get_fpstack : ctxt -> fpstack
val get_cc : ctxt -> ccinfo
val get_var_map : ctxt -> (identifier,kind) Dict.dict
val get_cap : ctxt -> con
val get_prop : ctxt -> con
val get_mode : ctxt -> mode

val set_mode : ctxt -> mode -> ctxt
val set_loc : ctxt -> tal_loc -> ctxt
val set_verify_ctxt : ctxt -> string -> ctxt
val error_handler : ctxt -> (ctxt -> verify_error -> unit) -> ctxt

val add_con : ctxt -> identifier -> kind -> ctxt
val add_con_def : ctxt -> identifier -> int_con_def -> ctxt
val add_var : ctxt -> identifier -> kind -> ctxt
val clear_vars : ctxt -> ctxt;; (* nukes all type variables in context *)
  (* set_abbrevs: assume top level so all type variables are nuked *)
val set_abbrevs : ctxt -> (identifier,con) Dict.dict -> ctxt
val add_abbrev : ctxt -> identifier -> con -> ctxt
val add_abbrevs : ctxt -> (identifier,con) Dict.dict -> ctxt
val add_local_subst :
  (con -> identifier -> con -> con)  (* must supply subst function from Talcon *)
   -> ctxt -> identifier -> con -> ctxt
val add_val : ctxt -> identifier -> con option -> mode -> ctxt
val set_val : ctxt -> identifier -> con option -> mode -> ctxt (* overwrites if present *)
val add_reg : ctxt -> reg -> con -> ctxt
val set_machine_state : ctxt -> machine_state -> ctxt
val set_fpstack : ctxt -> fpstack -> ctxt
val set_cc : ctxt -> ccinfo -> ctxt
val restore_cc : ctxt -> unit;; (* destructive *)
val set_cap : ctxt -> con -> ctxt
val add_conjunct : ctxt -> con -> ctxt
val set_prop : ctxt -> con -> ctxt

(* ---- LX ---- *)
(* refine the context with and abbreviation *)
val add_abbrev_map : ctxt -> (con -> con) -> identifier -> con -> bool -> ctxt 

val get_polarity : ctxt -> bool 
val get_kindvars : ctxt -> (identifier, bool) Dict.dict
val get_kindabbrevs : ctxt -> (identifier,kind) Dict.dict
val add_kindabbrev : ctxt -> identifier -> kind -> ctxt
val add_kindabbrevs : ctxt -> (identifier,kind) Dict.dict -> ctxt
val add_kind : ctxt -> identifier -> ctxt
val valid_kindvar : ctxt -> identifier -> unit
val check_kindvar : ctxt -> identifier -> bool
val reverse_polarity : ctxt -> ctxt
(* -- end LX -- *)


(* EOF: talctxt.mli *)
