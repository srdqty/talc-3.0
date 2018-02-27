(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Objfile
 * Object files common internal format.
 * Produced by talasm outputted by coff & elf.
 *)

open Numtypes;;
open Identifier;;
open Tal;;

type symbol_scope = Local | Weak | Global;;

(* symbol:
 *   for an external symbol, location values are 0 and sym_section is None.
 *)

type symbol = {
   sym_ident : identifier;
   mutable sym_index : int;		(* The index of this symbol, or 0. *)
 
   mutable sym_section : int;		(* Target section, or 0 if external *)
   mutable sym_offset : int;		(* Offset in target section, or 0 *)
   mutable sym_size : int;		(* Size of symbol, or 0 if external *)
   mutable sym_scope : symbol_scope	(* Local, Weak, or Global *)
 }
;;

type symbol_dict = (identifier, symbol) Dict.dict;;

type relativity = Relative | Absolute;;

type relocation = {
   rel_pos : int;			(* Virtual position of reference *)
   rel_relativity : relativity;		(* Relative or absolute? *)
   rel_scale : scale;			(* 8-, 16-, or 32-bit*)
   rel_force_public : bool;		(* Force relocation to be public? *)
   rel_symbol : symbol;			(* Reference is to this symbol *)
   rel_addend : int32			(* Refer to symbol + this addend *)
 }
;;

type section = {
   s_name : string;
   s_data : Stringchan.string_chan;		
   mutable s_relocs : relocation list;
   mutable s_symbols : symbol list 
 }
;;

type objfile = {
   o_secs : section list;
   mutable o_import_symbols : symbol list;
   o_symbol_dict : symbol_dict
 }
;;

(***** Constructors *****)

(* make_section name data relocs symbols
   Returns a new section. *)

val make_section :
    string -> Stringchan.string_chan -> relocation list -> symbol list ->
      section
;;

(* make_objfile secs imports symdict
   Returns a new object file. *)

val make_objfile :
    section list -> symbol list -> symbol_dict -> objfile
;;

(***** Symbol tables *****)

(* empty_symbol_table
   Returns a new, empty symbol table. *)

val empty_symbol_table : unit -> symbol_dict;;

(* make_internal_symbol ident sec_num offset size
   Returns a symbol for an internal reference. *)

val make_internal_symbol : identifier -> int -> int -> int -> symbol;;

(* make_external_symbol
   Returns a symbol for an external reference. *)

val make_external_symbol : identifier -> symbol;;

(* insert_symbol_in_dict r_sym_dict sym
   Side effects:
     - Inserts a symbol into the symbol dict. *)

val insert_symbol_in_dict : symbol_dict ref -> symbol -> unit;;

(* insert_symbol r_sym_dict r_sym_list sym
   Side effects: 
     - Inserts a symbol into the symbol dict and symbol list, both passed as 
       references. *)

val insert_symbol : symbol_dict ref -> symbol list ref -> symbol -> unit;;

(* lookup_symbol symdict ident
   Returns the symbol with identifier ident in the symbol dictionary. *)

val lookup_symbol : symbol_dict -> identifier -> symbol;;

(** Special symbols **)
(* When we receive a special import label, we take away its unique identifier
   number and use a generic label instead. *)

(* val array_bounds_error_label_name : string *)
val division_by_zero_error_label_name : string
val gc_malloc_label_name : string
(* set to true to use GC_malloc_debug rather than GC_malloc *)
val gc_debug : bool
val out_of_memory_error_label_name : string
val taltla_label_name : string

(* val array_bounds_error_label : identifier *)
val division_by_zero_error_label : identifier
val gc_malloc_label : identifier
val out_of_memory_error_label : identifier
val taltla_label : identifier

val special_labels : (identifier * con) list

(***** Relocations *****)

(* make_reloc pos rel scale sym offset public
   Returns a new relocation. *)
val make_reloc :
    int -> relativity -> scale -> symbol -> int32 -> bool -> relocation
;;

(* EOF: objfile.mli *)
