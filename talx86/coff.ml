(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Coff
 * Outputs coff format object files.
 *)

(* Changes:
 
   RLS 2/28/99: Added support for Cyclone ".cyc" sections. *)
   
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Objfile;;
open Binout;;

(********************************** TYPES ************************************)

(** Headers **)

type coff_file_header = {
    h_magic : int;
    h_num_sections : int;
    h_time_date : int;
    h_sym_table_offset : int;
    h_sym_table_entries : int;
    h_optional_header_size : int;
    h_flags : int
  }
;;

let i386_magic_number = 0x14C;;
let shflags_textsection = (0x6030, 0x0020)(* Code, 4-byte align, -rx *)
and shflags_datasection = (0xC030, 0x0040)(* Data, 4-byte align, wr- *)

(* Cyclone *)
and shflags_cyclonesection = (0x6030, 0x0060)
(* End Cyclone *)
;;

let sec_alignment = 32;; (* Align sections on a 32-byte boundary. *)
let sec_align = align_nbytes sec_alignment ;;
let sec_pad = pad_to_nbytes sec_alignment;;

type coff_section_header = {
    sh_section_name : string;  (* 8 chars -- null pad *)
    sh_physical_address : int;
    sh_virtual_address : int;
    sh_section_size : int;
    sh_section_offset : int;
    sh_relocation_offset : int;
    sh_line_number_offset : int;
    sh_num_relocation_entries : int;
    sh_num_line_number_entries : int;
    sh_flags : (int * int)		(* Pair is so we can use all 32 bits *)
  }
;;

(** Symbols **)

type symbol_name = 
   Short_Name of string			(* 8 bytes *)
 | String_Table_Entry of int		(* offset into string table *)
;;

type coff_symbol = {
   coff_sym_name : symbol_name;
   coff_sym_value : int;
   coff_sym_section : int;
   coff_sym_type : int;
   coff_sym_class : int;
   coff_sym_aux : string		(* auxiliary entries as string *)
 }
;;

let symtype_null = 0
and symclass_external = 2
and symclass_static = 3
and symclass_weakext = 108
and symsection_undef = 0
;;

type coff_symbol_table = {
   mutable st_symbols : coff_symbol list;	
   mutable st_symcount : int		(* Number of symbols *)
 }
;;

type coff_string_table = {
   mutable st_strings : string list;
   mutable st_length : int		(* Total length of strs + 4 *)
 }
;;

(** Relocations **)

type coff_relocation = {
   coff_rel_pos : int;			(* Virtual position *)
   coff_rel_symindex : int;		(* Index of symbol referred to *)
   coff_rel_type : int			(* Rel/abs, 16/32-bit *)
 }
;;

let coff_reloctype_rel16 = 0x2
and coff_reloctype_rel32 = 0x14
and coff_reloctype_dir16 = 0x1
and coff_reloctype_dir32 = 0x6
;;

(** Sizes **)

let sizeof_fileheader = 20;;
let sizeof_optfileheader = 0;;
let sizeof_secheader = 40;;
let sizeof_reloc = 10;;
let sizeof_symbol = 18;;

(** COFF File Representation **)

type coff_file = {
   f_header : coff_file_header;
   f_sec_headers : coff_section_header list;
   f_sec_data : Stringchan.string_chan list;
   f_relocs : coff_relocation list list;(* List of section reloc lists. *)
   f_symbols : coff_symbol_table;
   f_strings : coff_string_table
 }
;;

(***** Utility functions *****)

(* section_length
   Returns the length of a section in the object file. *)

let section_length sec =
   (Stringchan.get_mark sec.s_data)
;;

(* iter3 fn list1 list2 list3
   Iterates a function with three list arguments (see List.iter2).
   iter3 fn [a1; ...; z1] [a2; ...; z2] [a3; ...; z3] is
      (fn a1 a2 a3); ...; (fn z1 z2 z3). 
   Raises Invalid_argument if the lists have different lengths. 
   Used for put_sections. *)
   
let rec iter3 fn list1 list2 list3 =
   match (list1, list2, list3) with
      ([], [], []) -> ()
    | ((h1 :: t1), (h2 :: t2), (h3 :: t3)) ->
	 (fn h1 h2 h3);
	 (iter3 fn t1 t2 t3)
    | _ -> invalid_arg "iter3: lists don't have same length."
;;

(***** String table functions *****)

(* empty_string_table
   Returns an empty string table. *)

let empty_string_table() = {
   st_strings = [];
   st_length = 4			(* 4 bytes for length count. *)
 }
;;

(* add_string 
   Returns: the starting offset of the string. 
   Side effects: Adds the string to s_table, followed by a 0 byte. 
     Strings are added in reverse order. *)

let add_string strtable s =
   let start_pos = strtable.st_length in
   strtable.st_strings <- ((s ^ "\000") :: strtable.st_strings);
   strtable.st_length <- strtable.st_length + (String.length s) + 1;
   start_pos
;;

(***** Symbol table functions *****)

(* make_coff_symbol_name
   Returns a symbol_name for the given name string. 
   Side effects: adds the name to strtable if it is longer than 8 chars. *)

let make_coff_symbol_name strtable name = 
   if (String.length name) <= 8 then
      (Short_Name name)
   else
      (String_Table_Entry (add_string strtable name))
;;

(* make_coff_import_symbol
   Returns a symbol for an import (external, from undefined section.) *)

let make_coff_import_symbol name = {
   coff_sym_name = name;
   coff_sym_value = 0;
   coff_sym_section = symsection_undef;
   coff_sym_type = symtype_null;
   coff_sym_class = symclass_external;
   coff_sym_aux = ""
 }
;;

(* coff_symbol_scope
   Given an objfile symbol scope, returns its COFF equivalent. *)

let coff_symbol_scope scope =  
   match scope with
      Global -> symclass_external
    | Weak -> symclass_weakext
    | Local -> symclass_static
;;

(* make_coff_internal_symbol
   Returns a symbol for an internal (static or export) symbol. *)

let make_coff_internal_symbol name sec_num pos scope = {
   coff_sym_name = name;
   coff_sym_value = pos;
   coff_sym_section = sec_num;
   coff_sym_type = symtype_null;
   coff_sym_class = (coff_symbol_scope scope);
   coff_sym_aux = ""
 }
;;

(* make_coff_symbol
   Returns a new COFF symbol for the given objfile symbol.
   Side effects: 
     - Adds the symbol's name to strtable if it is longer than
       8 characters. *)

let make_coff_symbol strtable sym = 
   let sym_name = (make_coff_symbol_name strtable 
			(id_to_string sym.sym_ident)) in
   if sym.sym_section = 0 then
      (make_coff_import_symbol sym_name)
   else
      (make_coff_internal_symbol sym_name sym.sym_section sym.sym_offset
	    sym.sym_scope)
;;

(***** Symbol table functions *****)

(* empty_coff_symbol_table
   Returns an empty COFF symbol table. *)
let empty_coff_symbol_table() = {
   st_symbols = [];
   st_symcount = 0
 }
;;

(* add_coff_symbol
   Side effects: adds the given COFF symbol to the symbol table. *)

let add_coff_symbol symtable coff_sym =
   symtable.st_symbols <- (coff_sym :: symtable.st_symbols);
   symtable.st_symcount <- (symtable.st_symcount + 1)
;;

(* add_coff_symbols 
   Side effects: 
     - Translates each of the symbols in syms into a COFF symbol,
        and adds it to the symbol table. 
     - Sets the old symbol's index to its new index in the symbol table. *)

let rec add_coff_symbols symtable strtable syms =
   match syms with
      [] -> ()
    | (sym :: tail) ->
	 sym.sym_index <- symtable.st_symcount;
	 (add_coff_symbol symtable (make_coff_symbol strtable sym));
	 (add_coff_symbols symtable strtable tail)
;;

(* add_filename_symbol
   Side effects: adds the required filename symbol, and its auxiliary symbol
   entries, to the symbol table. *)

let add_filename_symbol symtable filename =
   let symclass_file = 103
   and symsection_filesection = 65534 in
   let filename_symbol = {
      coff_sym_name = (Short_Name ".file");
      coff_sym_value = 0;
      coff_sym_section = symsection_filesection;
      coff_sym_type = symtype_null;
      coff_sym_class = symclass_file;
      coff_sym_aux = filename
    } in
   begin
     (add_coff_symbol symtable filename_symbol);
     symtable.st_symcount <- (symtable.st_symcount + 
				((String.length filename) /+ sizeof_symbol))
   end
;;
      
(* add_section_start_symbol
   Side effects: adds a section's required section start symbol to the
   symbol table. *)

let add_section_start_symbol symtable sec_num sec =
   let section_aux = (Stringchan.create sizeof_symbol) in
   (Stringchan.put_4bytes section_aux (section_length sec));
   (Stringchan.put_2bytes section_aux (List.length sec.s_relocs));
   (Stringchan.put_2bytes section_aux 0);(* # of line numbers *)
   
   let section_symbol = {
      coff_sym_name = (Short_Name sec.s_name);
      coff_sym_value = 0;
      coff_sym_section = sec_num;
      coff_sym_type = 0;
      coff_sym_class = symclass_static;
      coff_sym_aux = (Stringchan.to_string section_aux)
    } in
   (add_coff_symbol symtable section_symbol);
   symtable.st_symcount <- (symtable.st_symcount + 1)
;;

(* add_section_symbols
   Side effects: adds the section start symbol and all of the symbols
   for the section into the symbol table. *)

let rec add_section_symbols symtable strtable sec_num secs =
   match secs with
      [] -> ()
    | (sec :: tail) ->
	 (add_section_start_symbol symtable sec_num sec);
	 (add_coff_symbols symtable strtable sec.s_symbols);
	 (add_section_symbols symtable strtable (sec_num + 1) tail)
;;

(* add_import_symbol
   Side effects: 
     - Translates the objfile symbol into a COFF import symbol,
       and adds it to the symbol table. 
     - Sets the index of the old objfile symbol to its new location. *)

let rec add_import_symbol symtable strtable sym =
   sym.sym_index <- symtable.st_symcount;
   (add_coff_symbol symtable (make_coff_import_symbol 
			      (make_coff_symbol_name strtable 
				    (id_to_string sym.sym_ident))))
;;

(* reverse_symbol_table
   Returns a symbol table with the symbols in the proper order, when
   given a symbol table with the symbols in reverse order. *)

let reverse_symbol_table symtable = {
   st_symcount = symtable.st_symcount;
   st_symbols = (List.rev symtable.st_symbols)
 }
;;

(* make_coff_symbol_table  
   Returns a COFF symbol table for the given objfile, with all COFF-specific
     symbols added. 
   Side effects: 
     - Sets strtable to the string table for the symbol table. 
     - Each objfile symbol in the symbol dictionary is given its
       actual index in the symbol list.*)

let make_coff_symbol_table strtable srcfilename objfile = 
   let symtable = empty_coff_symbol_table() in
   (add_filename_symbol symtable srcfilename);
   (add_section_symbols symtable strtable 1 objfile.o_secs);
   (List.iter (add_import_symbol symtable strtable) objfile.o_import_symbols);
					(*Strtable and symtable are backward!*)
   strtable.st_strings <- (List.rev strtable.st_strings);
   (reverse_symbol_table symtable)
;;

(***** Relocation table functions *****)

(* coff_reloc_type_of
   Returns the COFF relocation type for the given relocation relativity
   and scale. *)

let coff_reloc_type_of relativity scale =
   match (relativity, scale) with
      (Relative, Byte2) -> coff_reloctype_rel16
    | (Relative, Byte4) -> coff_reloctype_rel32
    | (Absolute, Byte2) -> coff_reloctype_dir16
    | (Absolute, Byte4) -> coff_reloctype_dir32
    | _ -> failwith "coff.ml: coff_reloc_type_of: bad relocation type"
;;

(* write_reloc_addend
   Side effects: If the relocation has an addend, writes it as an implicit
   addend in the code. *)

let write_reloc_addend secdata reloc = 
   if reloc.rel_addend <>$ i32_0 then
      begin
	 let savemark = (Stringchan.get_mark secdata) in
	 (Stringchan.set_mark secdata reloc.rel_pos);
	 begin
	    match reloc.rel_scale with
	       Byte2 ->
		 let (b1,b2) = int32_to_2bytes reloc.rel_addend in
		 Stringchan.put_char secdata b1;
		 Stringchan.put_char secdata b2
	     | Byte4 ->
		 let (b1,b2,b3,b4) = int32_to_4bytes reloc.rel_addend in
		 Stringchan.put_char secdata b1;
		 Stringchan.put_char secdata b2;
		 Stringchan.put_char secdata b3;
		 Stringchan.put_char secdata b4
	     | _ -> failwith "write_reloc_addend @ coff.ml: reloc scale"
	 end;
	 (Stringchan.set_mark secdata savemark);
      end
;;

(* make_coff_relocation
   Returns a new COFF relocation from the given objfile relocation. *)

let make_coff_relocation reloc = {
   coff_rel_pos = reloc.rel_pos;
   coff_rel_symindex = reloc.rel_symbol.sym_index;
   coff_rel_type = (coff_reloc_type_of reloc.rel_relativity reloc.rel_scale)
 }
;;

(* make_section_reloctable
   Returns a list of relocations for the given section. *)

let make_section_reloctable sec =
   (List.iter (write_reloc_addend sec.s_data) sec.s_relocs);
   (List.map make_coff_relocation sec.s_relocs)
;;

(* make_coff_reloctable
   Returns a list of lists of relocations. Each list of relocations belongs
   to the corresponding section in the section list. *)

let make_coff_reloctable objfile =
   (List.map make_section_reloctable objfile.o_secs)
;;

(***** Section data functions *****)

(* get_section_data
   Returns the data associated with the given section. *)
 
let get_section_data sec =
   sec.s_data
;;

(* get_data
   Returns a list of string channels containing the data for each section. *)

let get_data objfile = 
   (List.map get_section_data objfile.o_secs)
;;

(***** Section header functions *****)

(* header_flags
   Returns the flag values for a given section. *)

let header_flags sec = 
   match sec.s_name with
      ".text" ->
	 shflags_textsection
(* Cyclone *)
    | ".cyc" ->
	 shflags_cyclonesection
(* End Cyclone *)
    | ".data" ->
	 shflags_datasection
    | _ ->
	 failwith "coff.ml: header_flags: unknown section name."
;;

(* make_sec_header
   Returns a header for the given section. 
   This calculates the length of the section and the positions of all its
   data fields.*)

let make_sec_header sec_offset sec_length sec_reloc_count sec = {
   sh_section_name = sec.s_name;
   sh_physical_address = 0;
   sh_virtual_address = 0;
   sh_section_size = sec_length;
   sh_section_offset = sec_offset;
   sh_relocation_offset = sec_offset + sec_length;
   sh_line_number_offset = 0;
   sh_num_relocation_entries = sec_reloc_count;
   sh_num_line_number_entries = 0;
   sh_flags = (header_flags sec)
 }
;;

(* make_sec_headers
   Returns a list of section headers, one for each section. 
   Side effects: sets r_endpos to the position of the end of the data,
   i.e., the start of the symbol table. *)

let rec make_sec_headers r_endpos startpos secs =
   match secs with
      [] -> (r_endpos := startpos; [])
    | (sec :: tail) ->
	 let sec_numrelocs = (List.length sec.s_relocs) in
	 let sec_length = (section_length sec) in
	 let sec_endpos = 
	   sec_align (startpos + sec_length + sec_numrelocs * sizeof_reloc) in
	 (make_sec_header startpos sec_length sec_numrelocs sec)
	 :: (make_sec_headers r_endpos sec_endpos tail)
;;

(***** COFF File Header *****)

let get_unix_time () = 
   0					(**)(* For now, returns 0 as time. *)
;;

let make_coff_file_header num_sections num_symbols sym_table_start = {
   h_magic = i386_magic_number;
   h_num_sections = num_sections;
   h_time_date = (get_unix_time());
   h_sym_table_offset = sym_table_start; 
   h_sym_table_entries = num_symbols;
   h_optional_header_size = sizeof_optfileheader;
   h_flags = 0
 }
;;
   
(* make_coff
   Creates a COFF file object from the given objfile. *)

let make_coff srcfilename objfile =
   let num_sections = (List.length objfile.o_secs) in
   let strings = empty_string_table() in
   let symbols = (make_coff_symbol_table strings srcfilename objfile) in
   let relocs = (make_coff_reloctable objfile) in
   let data = (get_data objfile) in
   let data_start = (sizeof_fileheader + sizeof_optfileheader
       		     + (num_sections * sizeof_secheader)) in
   let r_sympos = ref 0 in
   let sec_headers = (make_sec_headers r_sympos data_start objfile.o_secs) in
   let header = (make_coff_file_header num_sections symbols.st_symcount
		      !r_sympos) in
   {  f_header = header;
      f_sec_headers = sec_headers;
      f_sec_data = data;
      f_relocs = relocs;
      f_symbols = symbols;
      f_strings = strings
    }
;;

(*************************** Writing the COFF File ***************************)

(* put_file_header
   Outputs a COFF file header. *)

let put_file_header chan header =
   begin 
      put_hword chan header.h_magic;
      put_hword chan header.h_num_sections;
      put_word chan header.h_time_date;
      put_word chan header.h_sym_table_offset;
      put_word chan header.h_sym_table_entries;
      put_hword chan header.h_optional_header_size;
      put_hword chan header.h_flags;
   end
;;

(* put_sec_header
   Outputs a section header. *)

let put_sec_header chan sh =
   begin
      put_extend chan sh.sh_section_name 0 8;
      put_word chan sh.sh_physical_address;
      put_word chan sh.sh_virtual_address;
      put_word chan sh.sh_section_size;
      put_word chan sh.sh_section_offset;
      put_word chan sh.sh_relocation_offset;
      put_word chan sh.sh_line_number_offset;
      put_hword chan sh.sh_num_relocation_entries;
      put_hword chan sh.sh_num_line_number_entries;
      put_hword chan (snd sh.sh_flags);
      put_hword chan (fst sh.sh_flags);
   end
;;

(* put_sec_headers
   Outputs the header of each section in the COFF file. *)

let put_sec_headers chan headers =
   (List.iter (put_sec_header chan) headers)
;;

(* put_sec_data
   Outputs section data. *)

let put_sec_data chan data =
   (put_string_len chan (Stringchan.to_string data) 0 
	 (Stringchan.get_mark data))
;;

(* put_reloc
   Outputs a relocation. *)

let put_reloc chan reloc = 
   begin
      put_word chan reloc.coff_rel_pos;
      put_word chan reloc.coff_rel_symindex;
      put_hword chan reloc.coff_rel_type
   end
;;

(* put_section
   Outputs a section's data and relocation table. *)

let put_section chan header data relocs =
   (assert_at chan header.sh_section_offset "section data");
   (put_sec_data chan data);		(* Write section data *)
   
   (assert_at chan header.sh_relocation_offset "section relocation table");
   (List.iter (put_reloc chan) relocs);	(* Write section relocs *)

   (* Pad to align to 4 bytes. *)
  sec_pad chan;
  ()
;;


(* put_sections
   Outputs the data and relocation tables of each section in the coff file. *)

let put_sections chan coff =
   (iter3 (put_section chan) coff.f_sec_headers coff.f_sec_data
	 coff.f_relocs)
;;

(* put_symbol_name
   Outputs a symbol name, or an 8-byte index of a string in the string table.*)

let put_symbol_name chan name =
   match name with
      Short_Name s -> (put_extend chan s 0 8)
    | String_Table_Entry i -> 
	 (put_4bytes chan 0);
	 (put_4bytes chan i)
;;

(* put_symbol_aux
   Outputs an auxiliary symbol count, followed by the auxiliary symbols
   in the auxiliary symbol string, if they exist. *)

let put_symbol_aux chan aux_string =
   if aux_string = "" then 
      (put_byte chan 0)
   else
      begin
	 let aux_size = ((String.length aux_string) /+ sizeof_symbol) in
	 (put_byte chan aux_size);
	 (put_extend chan aux_string 0 (aux_size * sizeof_symbol));
      end
;;

(* put_symbol
   Outputs the symbol, followed by any auxiliary symbols. *)

let put_symbol chan sym =
   (put_symbol_name chan sym.coff_sym_name);
   (put_word chan sym.coff_sym_value);
   (put_hword chan sym.coff_sym_section);
   (put_hword chan sym.coff_sym_type);
   (put_byte chan sym.coff_sym_class);
   (put_symbol_aux chan sym.coff_sym_aux)
;;
   
(* put_symbol_table
   Outputs the symbol table. *)

let put_symbol_table chan where symtable =
   (assert_at chan where "symbol table");
   (List.iter (put_symbol chan) symtable.st_symbols)
;;

(* put_string_table
   Outputs the string table. *)

let put_string_table chan strtable =
   (put_word chan strtable.st_length);
   (List.iter (put_string chan) strtable.st_strings)
;;

(* write_coff
   Side effects: writes the data from the file structure to outfile. *)

let write_coff outfile coff =
   (put_file_header outfile coff.f_header);
   (put_sec_headers outfile coff.f_sec_headers);
   (put_sections outfile coff);
   (put_symbol_table outfile coff.f_header.h_sym_table_offset coff.f_symbols);
   (put_string_table outfile coff.f_strings)
;;

(* create_coff 
   Side effects : Opens a file with the given name, produces a COFF 
   file structure from the object file, and writes it. *)

let create_coff srcfilename outfilename objfile =
   let outfile = (open_out_bin outfilename) in
   (write_coff outfile (make_coff srcfilename objfile));
   (close_out outfile)
;;

(* EOF: coff.ml *)
