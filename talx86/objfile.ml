(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Objfile
 * Object files common internal format.
 * Produced by talasm outputted by coff & elf.
 *)

(* Changes:
   RLS 3/3/99: "Dict.Absent" exceptions are now rethrown with more detail. *)

open Numtypes;;
open Identifier;;
open Tal;;

(***** Types *****)

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

(* make_section
   Returns a new section. *)

let make_section name data relocs symbols = {
   s_name = name;
   s_data = data;
   s_relocs = relocs;
   s_symbols = symbols
 }
;;

(* make_objfile
   Returns a new object file. *)

let make_objfile secs imports symdict = {
   o_secs = secs;
   o_import_symbols = imports;
   o_symbol_dict = symdict
 }
;;

(***** Symbol tables *****)

(* empty_symbol_table
   Returns a new, empty symbol table. *)

let empty_symbol_table () = (Dict.empty id_compare);;


(* make_internal_symbol
   Returns a symbol for an internal reference. *)

let make_internal_symbol ident section offset size = {
   sym_ident = ident;
   sym_section = section;
   sym_offset = offset;
   sym_size = size;
   sym_scope = Local;
   sym_index = 0
 }
;;

(* make_external_symbol
   Returns a symbol for an external reference. *)

let make_external_symbol ident = {
   sym_ident = ident;
   sym_section = 0;
   sym_offset = 0;
   sym_size = 0;
   sym_scope = Global;
   sym_index = 0
 }
;;

(* insert_symbol_in_dict 
   Side effects:
     - Inserts a symbol into the symbol dict. *)

let insert_symbol_in_dict r_sym_dict sym =
   try 
     r_sym_dict := (Dict.insert_new !r_sym_dict sym.sym_ident sym)
   with Dict.Present ->
     failwith "Symbol is already present in symbol table."
;;   

(* insert_symbol 
   Side effects: 
     - Inserts a symbol into the symbol dict and symbol list, both passed as 
       references. *)

let insert_symbol r_sym_dict r_sym_list sym =
   (insert_symbol_in_dict r_sym_dict sym);
   r_sym_list := sym :: !r_sym_list
;;

(* lookup_symbol 
   Returns the symbol with identifier 'ident' in the symbol dictionary. *)

let lookup_symbol symdict ident = 
   try 
      (Dict.lookup symdict ident)
   with
      Dict.Absent -> failwith ("lookup_symbol: '" ^ 
				 (Identifier.id_to_string ident) ^ "' absent")
;;

(** Special labels **)
let gc_debug = false
(* let array_bounds_error_label_name = "_array_bounds_error";; *)
let division_by_zero_error_label_name = "_division_by_zero_error";;
let gc_malloc_label_name = 
  if gc_debug then "_debug_malloc" else "_GC_malloc";;
let out_of_memory_error_label_name = "_out_of_memory";;
let taltla_label_name = "_taltla";;

(* let array_bounds_error_label = (id_of_string array_bounds_error_label_name) *)
let division_by_zero_error_label = (id_of_string division_by_zero_error_label_name)
and gc_malloc_label = (id_of_string gc_malloc_label_name)
and out_of_memory_error_label = (id_of_string out_of_memory_error_label_name)
and taltla_label = (id_of_string taltla_label_name)
;;

let special_labels = [  (* (array_bounds_error_label, cempty); *)
                        (* (division_by_zero_error_label_name, cempty); *)
			(gc_malloc_label, cempty);
			(out_of_memory_error_label, cempty);
		        (taltla_label, cempty) ]
;;

(***** Relocations *****)

(* make_reloc
   Returns a new relocation. *)

let make_reloc pos rel scale sym offset public = {
   rel_pos = pos;
   rel_relativity = rel;
   rel_scale = scale;
   rel_addend = offset;
   rel_force_public = public;
   rel_symbol = sym
 }
;;

(* EOF: objfile.ml *)
