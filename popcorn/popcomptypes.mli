(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,Frederick Smith    *)
(*     Dan Grossman                                                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Prior to Release 1.7, this was part of Popcompile; now it is opened by
   that module.
   Here we put all type compilation that doesn't require a code generation
   environment.
 *)

type id    = Identifier.identifier
type typ   = Popsyntax.typ
type con   = Tal.con
type int32 = Numtypes.int32

(* exceptions used throughout code generation *)
exception Unimplemented of string
exception Impossible    of string
exception Void_Type
val unimpl : string -> 'a
val impos  : string -> 'a

(* size of the translation of a popcorn type *)
(* assumes: all type variables are 4-bytes wide *)
val words_in_con : con -> int

(* convert to tal identifiers *)
val tid_val  : string -> id
val tid_fun  : Popsyntax.convention -> int -> string -> id (*int = size of args in bytes*)
val tid_mem  : string -> id
val tid_type : string -> id
val tid_tyv  : string -> id
val tid_exn  : string -> id
val tid_name : string -> id
val tid_internal : string -> id

(* type variable utilities *)
val tyvars_to_cons : string list -> con list
val tyvars_to_lam  : string list -> con      -> con
val tyvars_to_kind : 'a list     -> Tal.kind -> Tal.kind

(* cons and abbrevs *)
val std_abbrevs : (id * con) list (* All the standard abbreviations. *)

val stack1_v              : id
val stack2_v              : id
val cap1_c                : con
val cap2_c                : con


val stack1_c          : con
val stack2_c          : con
val exn_con           : con
val exnname_arg_con   : id  -> con
val exnname_con       : con -> con
val exnname_arg_con_of: con -> con
val int_con           : con
val bool_con          : con
val char_con          : con
val string_con        : con
val opt_con           : con -> con
val array_con         : con -> con
(* LR *)
val rep_con           : con -> con
(* end LR *)
val exn_stack_con     : con -> con -> con
val handle_con        : con -> con -> con
val array_real_con    : con -> con
val handler_con       : con -> con -> con -> con -> con
val bogus_option_con  : con

val exn_body          : id  -> con

(* con utilities *)
val name_con     : string -> con list -> con
val mem_name_con : string -> con list -> con
val close_code   : id list -> string list -> con -> con

(* con predicates *)
val is_exnname_con : con -> bool

(* type translation *)
val cap2var       : Popsyntax.capability -> Tal.variance
val typ2con       : typ -> con
val fun_con'      : Popsyntax.convention -> Popsyntax.var list -> typ -> con list -> con * con * con
val fun_con       : Popsyntax.convention -> Popsyntax.var list -> typ -> typ list -> con * con * con
val types2cons    : typ list -> con list
val mallocarg2con : Tal.mallocarg -> con
val typ2mallocfield : typ -> Tal.mallocarg
val con2field : con -> con

val bogus_option_con_block : id * Tal.kind * con

 (* To turn off annotation hack, use this instead of following and
    modify popcompile.ml popcomptypes.ml as directed there *)
(* val fun_coercion   : con -> con -> Tal.coercion -> con list -> 
                       Tal.coercion list *)
val fun_coercion   : int -> int -> con -> (* Stack *)
  con -> con -> (* Capabilities *)
      con list -> Tal.coercion list (* Argument types *)

val needs_indirect : typ -> bool

val get_name : typ -> Popsyntax.type_name

(* struct information *)
type struct_info
val info_structdecl     : Popsyntax.structdecl -> struct_info
val import_struct_info  : string -> (string list) -> 
                            (Identifier.identifier * Tal.con) list -> 
			    struct_info
val struct_field_offset : struct_info -> Popsyntax.field_name -> int
val struct_field_offsets: struct_info -> int list
val struct_field_cons   : struct_info -> con list
val roll_struct         : struct_info -> string -> con list -> Tal.coercion list
val struct_null         : struct_info -> bool
val struct_t            : struct_info -> con * Tal.kind
val struct_mem_t        : struct_info -> con * Tal.kind

(* union information *)
type union_info
val info_uniondecl       : Popsyntax.uniondecl -> union_info
val union_t              : union_info -> con * Tal.kind
val union_num_voids      : union_info -> int
val union_void_tag_assoc : union_info -> Popsyntax.field_name -> int32
val union_val_tag_assoc  : union_info -> Popsyntax.field_name -> int32 * con
val union_instantiate    : union_info -> con -> con list -> con

(* abstype information *)
type abstype_info
val info_abstype       : Popsyntax.absdecl -> abstype_info
val abstype_t          : abstype_info -> con * Tal.kind
val abs_pack_coercions : 
    abstype_info -> Popsyntax.typ list -> 
      Popsyntax.typ list -> Tal.coercion list

(* other *)
val bool        : bool -> Tal.genop * Tal.coercion list
val no_effect   : Popsyntax.exp -> bool
val no_effect_l : Popsyntax.exp list -> bool
val exp2typ     : Popsyntax.exp -> typ
val exp2con     : Popsyntax.exp -> con
