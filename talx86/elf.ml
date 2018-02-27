(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Elf
 * Outputs elf format object files.
 *)

(* Notes:
   
***** Underscores *****   (RLS 4/22/99)
   Global symbols in ELF files are not prefixed with an underscore, while
   they are in COFF files and under the general framework of TAL. 
 * Quick fix (applied): The prefixing underscore is removed from each symbol
   using rm_underscore before the symbol is printed to the symbol table.
 * NOTE: To use a symbol that really has an underscore preceding it, such as 
   _IO_stdin_, you must now prefix it with two underscores: __IO_stdin_.
   But since system variables like this aren't defined in TAL, this shouldn't
   affect most uses.
 * Better fix: Have a framework where underscores are put on the
   symbol names appropriately from the start.

More on this: (MWH 9/29/99)
  Since we only remove underscores from some symbols, there needs to be
  a way to tell the disassembler which symbols were changed.  We do this
  by placing a 1 (rather than a 0) in the st_other field of the symbol.
  This field is not defined to have any use in the current ELF
  standard.  This is definitely a hack, but what else you gonna do
  (see Better fix suggestion above---ouch) ?

*****

***** Relocation offsets ***** (RLS 4/22/99)
   In ELF, relocations for jumps are relative to PC, but Intel jumps are
   taken relative to PC+4. 
 * Quick fix (applied): As a consequence, an addend of -4 is
   applied to each PC-relative relocation. (see to_elf_relocation)
   This is actually what most Intel ELF compilers do.
 * Better fix: Write the relocations with implicit addends when the labels
   are written by the internal linker, in talasm.ml.
*****
*)   
   
open Numtypes;;
open Identifier;;
open Tal;;
open Objfile;;
open Binout;;

(***** File header *****)

type elf_file_header = {
   ei_magic : string;			(* len = 4. use ei_mag = "\127ELF" *)
   ei_class : int;			(* b: use elfclass32 = 1 *)
   ei_data : int;			(* use elfdata2lsb = 1 for big-endian*)
   ei_version : int;			(* byte: use ev_current = 1 *)
					(* Skip to 0+16 bytes *)
   e_type : int;			(* hword: file type *)
   e_machine : int;			(* hword: target machine *)
   e_version : int;			(* word: use ev_current = 1 *)
   e_entry : int;			(* word: entry point *)
   e_phoff : int;			(* word: program header offset *)
   e_shoff : int;			(* word: section header offset *)
   e_flags : long;			(* word: flags *)
   e_ehsize : int;			(* hword: ELF header size *)
   e_phentsize : int;			(* hword: prog header entry size *)
   e_phnum : int;			(* hword: # of prog header entries *)
   e_shentsize : int;			(* hword: sec header entry size *)
   e_shnum : int;			(* hword: # of sec header entries *)
   e_shstrndx : int		 (* hword: section name string table index *)
 }
;;

let sizeof_elf_file_header = 52;;

(* Values for e_magic, e_class, e_data, e_version *)
let ei_magic_value = "\127ELF";;
let elfclass32 = 1;;
let elfdata2lsb = 1;;
let ev_current = 1;;

(* Values for e_type *)
let et_rel = 1				(* Relocatable file *)
and et_exec = 2				(* Executable *)
and et_dyn = 3  			(* DLL *)
;;

(* Values for e_machine *)
let em_386 = 3
and em_486 = 6
and em_ppc = 20;;				

(***** Symbol table *****)

type elf_symbol = {
   st_name : int;			(*name of the symbol, put in strtable*)
   st_value : int;			(* word: offset of symbol in section *)
   st_size : int;			(* word: size *)
   st_binding : int;			(* 4bits: stb_* *)
   st_type : int;			(* 4bits: stt_* *)
   mutable st_other : int;		(* byte, reserved. *)
   st_shndx : int			(* hword: section num. of symbol *)
 }
;;

let sizeof_elf_symbol = 16;;

type elf_symbol_table = {
   mutable st_local_symbols : elf_symbol list;	
   mutable st_local_symcount : int;	(* Number of local symbols. *)
   mutable st_global_symbols : elf_symbol list;
   mutable st_symcount : int		(* Index of next symbol *)
 }
;;

type elf_string_table = {
   mutable st_strings : string list;
   mutable st_length : int		(* Index of next string *)
 }
;;

(***** Relocations *****)

type elf_relocation_entry = {
   r_offset : int;			(*W: where to apply relocation *)
				   (* The below are combined into r_info: *)
   r_sym : int;				(*H+B: the index of the symbol *)
   r_type : int;			(*B: the type of relocation *)
 }
;;

let sizeof_elf_reloc = 8;;
let sizeof_elf_rela = 12;;		(* Size of reloc with addend *)

(***** Section header *****)

type elf_section_link = 
   Int_link of int
 | Section_link of elf_section

and elf_section_header = {		(* All values are words. *)
   sh_name : int;			(* Index of name in section name tbl *)
   sh_type : int;
   sh_flags : int;
   sh_addr : int;
   mutable sh_offset : int;		(* Determined at write time *)
   sh_size : int;
   sh_link : elf_section_link;
   sh_info : elf_section_link;
   sh_addralign : int;
   sh_entsize : int
 }

(**** Sections *****)

and elf_section_data = 
 | Prog_data of Stringchan.string_chan
 | Relocation_data of elf_relocation_entry list
 | Symbol_data of elf_symbol_table
 | String_data of elf_string_table
 | No_data

and elf_section = {
   mutable es_number : int;
   es_header : elf_section_header;
   es_data : elf_section_data
 }
;;

let sec_alignment = 32;; (* Align sections on a 32-byte boundary *)
let sec_align     = align_nbytes sec_alignment;;
let sec_pad       = pad_to_nbytes sec_alignment;;

(***** ELF file internal representation *****)

type elf_file = {
   f_header : elf_file_header;
   f_sections : elf_section array
 }
;;

(***** Section header values *****)

let sizeof_elf_section_header = 40;;

(* section header numbers *)
let shn_undef = 0
and shn_abs = 0xFFF1			(* Symbols with absolute location *)
and shn_common = 0xFFF2  		(* Common symbols *)
;;

(* values for sh_name *)
let sname_comment = ".comment"		(* version info *)
and sname_data = ".data"		(* initialized data *)
and sname_dynamic = ".dynamic"		(* dynamic linking info *)
and sname_dynstr = ".dynstr"		(* strings for dynamic linking *)
and sname_dynsym = ".dynsym"		(* dynamic linking symbol table *)
and sname_fini = ".fini"		(* process termination code *)
and sname_got = ".got"			(* global offset table *)
and sname_hash = ".hash"		(* symbol hash table *)
and sname_init = ".init"		(* initialization code *)
and sname_note = ".note"		(* notes *)
and sname_plt = ".plt"			(* procedure linkage table *)
and sname_rel sname = ".rel" ^ sname	(* relocations for a named section *)
and sname_rela sname = ".rela" ^ sname	(* relocs with explicit addends *)
and sname_rodata = ".rodata"		(* read-only data *)
and sname_shstrtab = ".shstrtab"	(* section name string table *)
and sname_strtab = ".strtab"		(* string table *)
and sname_symtab = ".symtab"		(* symbol table *)
and sname_text = ".text"		(* executable instructions *)
;;

(* values for sh_type *)

let sht_null = 0
and sht_progbits = 1			(* information for the program *)
and sht_symtab = 2			(* symbol table *)
and sht_strtab = 3			(* string table *)
and sht_rela = 4			(*relocation table w/explicit addends*)
and sht_hash = 5			(* symbol hash table *)
and sht_dynamic = 6			(* dynamic linking info *)
and sht_note = 7			(* notes *)
and sht_nobits = 8			(* occupies no space in file *)
and sht_rel = 9				(* relocation table w/o addends *)
and sht_shlib = 10			(* unspecified *)
and sht_dynsym = 11 			(* dynamic symbol table *)
;;

let english_sh_type (i : int) =
   match i with
    | 0 -> "Null"
    | 1 -> "Program bits"
    | 2 -> "Symbol table"
    | 3 -> "String table"
    | 4 -> "Relocation table (explicit addends)"
    | 5 -> "Symbol hash table"
    | 6 -> "Dynamic info"
    | 7 -> "Notes"
    | 8 -> "No bits"
    | 9 -> "Relocation table (no explicit addends)"
    | 10 -> "Unspecified shared library stuff"
    | 11 -> "Dynamic symbol table"
    | _ -> "Unknown"
;;

(* values for sh_flags *)

let shf_write = 1			(* writable *)
and shf_alloc = 2			(* occupies memory *)
and shf_execinstr = 4  			(* executable *)
;;

(***** Symbol table values *****)

(* values for st_binding *)

let stb_local = 0
and stb_global = 1
and stb_weak = 2
;;

(* values for st_type *)

let stt_notype = 0			(* no type *)
and stt_object = 1			(* data object/variable *)
and stt_func = 2			(* function or code label *)
and stt_section = 3			(* section *)
and stt_file = 4  			(* name of source file *)
;;			    (* stt_file must have stb_local, be in shn_abs*)

(***** Constructors *****)

let make_elf_file_header shoffset shnum shstrndx = {
   ei_magic = ei_magic_value;
   ei_class = elfclass32;
   ei_data = elfdata2lsb;
   ei_version = ev_current;

   e_type = et_rel;
   e_machine = em_386;
   e_version = ev_current;
   e_entry = 0;
   e_phoff = 0;
   e_shoff = shoffset;
   e_flags = (0, 0);
   e_ehsize = sizeof_elf_file_header;
   e_phentsize = 0;
   e_phnum = 0;
   e_shentsize = sizeof_elf_section_header;
   e_shnum = shnum;
   e_shstrndx = shstrndx
 }
;;

let make_zero_section() = {
   es_number = 0;
   es_header = {   sh_name = 0;
		 sh_type = sht_null;
		 sh_flags = 0;
		 sh_addr = 0;
		 sh_offset = 0;
		 sh_size = 0;
		 sh_link = Int_link 0;
		 sh_info = Int_link 0;
		 sh_addralign = 0;
		 sh_entsize = 0
	       };
   es_data = No_data
 }
;;
 
(* string tables begin with \0 and end with \0, using \0-terminated strings. *)

let make_zero_symbol() = {
   st_name = 0;
   st_value = 0;
   st_size = 0;
   st_binding = stb_local;
   st_type = stt_notype;
   st_other = 0;
   st_shndx = shn_undef
 }
;;

(***** String table functions *****)

(* empty_string_table
   Returns an empty string table. *)

let empty_string_table () = {
   st_strings = [];
   st_length = 1			(* Skip 1 byte for initial \0 byte. *)
 }
;;

(* add_string 
   Returns: the starting offset of the string. 
   Side effects: Adds the string to strtable.
     Strings are added in reverse order. *)

let add_string strtable s =
   if (String.length s) > 0 then
      let start_pos = strtable.st_length in
      strtable.st_strings <- (s :: strtable.st_strings);
      strtable.st_length <- strtable.st_length + (String.length s) + 1;
      start_pos
   else
      0
;;

(* make_strtable_section
   Returns an ELF section for the given string table.
   Side effects: the section's name is put into secnames. *)

let make_strtable_section secnames strtable = {
   es_number = 0;			(* Determined at write time *)
   es_header = { sh_name = (add_string secnames sname_strtab);
		 sh_type = sht_strtab;
		 sh_flags = 0;
		 sh_addr = 0;
		 sh_offset = 0;		(* Determined at write time *)
		 sh_size = strtable.st_length;
		 sh_link = Int_link 0;
		 sh_info = Int_link 0;
		 sh_addralign = 1;
		 sh_entsize = 0
	       };
   es_data = String_data strtable
 }
;;

(***** Symbol table functions *****)


(* rm_underscore
   In ELF, labels by convention do not begin with an underscore. 
   Returns the string with the leading underscore removed so that the symbol
   table will be compatible with the symbol tables for our include files.

   This returns a pair consisting of an integer and the new string, where
   if int = 0 then string was unchanged, if int = 1 then underscore
   was removed.  This (0,1) value is stored in the info field of the
   symbol (currently unused) to indicate to the disassembler how to
   "un_rm_underscore" the symbols. *)

let rm_underscore s =
  if s.[0] = '_' then
    (1,String.sub s 1 ((String.length s) - 1))
  else
    (0,s)
;;


(* to_elf_st_binding
   Returns the appropriate binding value for a given symbol scope. *)

let to_elf_st_binding scope =
   match scope with
      Local -> stb_local
    | Weak -> stb_weak
    | Global -> stb_global
;;


(* make_elf_import_symbol
   Returns a symbol for an import (global, from undefined section.) 
   Side effects: adds the symbol's name to the string table. *)

let make_elf_import_symbol strtable symname = 
  let (other,name) = (rm_underscore symname) in
  { st_name = (add_string strtable name);
    st_value = 0;
    st_size = 0;
    st_binding = stb_global;
    st_type = stt_notype;
    st_other = other;
    st_shndx = shn_undef
  }
;;


(* is_code_symbol 
   Returns TRUE if the symbol is in a code section, not a data section. *)

let is_code_symbol sec_num pos =
   (sec_num = 2)
;;

(* make_elf_internal_symbol
   Returns a symbol for an internal (static or export) symbol. 
   Side effects: adds the symbol's name to the string table. *)

let make_elf_internal_symbol strtable name sec_num pos size scope = 
  let (other,name) = (rm_underscore name) in
  { st_name = (add_string strtable name);
    st_value = pos;
    st_size = size;
    st_binding = (to_elf_st_binding scope);
    st_type = (if (is_code_symbol sec_num pos) then stt_func else stt_object);
    st_other = other;
    st_shndx = sec_num
  }
;;

(* to_elf_symbol 
   Returns the given objfile symbol, translated into an ELF symbol. 
   Side effects: adds the symbol's name to the string table. *)

let to_elf_symbol strtable sym = 
   if sym.sym_section = 0 then
      (make_elf_import_symbol strtable (id_to_string sym.sym_ident))
   else
      (make_elf_internal_symbol strtable (id_to_string sym.sym_ident)
	    sym.sym_section sym.sym_offset sym.sym_size sym.sym_scope)
;;

(* empty_elf_symbol_table
   Returns an ELF symbol table containing the zero symbol. *)
let empty_elf_symbol_table() = {
   st_local_symbols = [make_zero_symbol()];
   st_local_symcount = 1;

   st_global_symbols = [];
   st_symcount = 1
 }
;;

(* add_elf_symbol
   Side effects: adds the given ELF symbol to the symbol table, in the local
   or the global section, as appropriate. 
   NOTE: The symbol lists are constructed in reverse order. 
   Returns: The symbol's number in the symbol list.

   MWH 10/30/99 -- The disassembler relies on knowing the order of the
   symbols as they appear in the .tal file, but this function separates
   the symbols into local and global tables, so that if a local and global
   symbol have the same address, their order will not be preserved.  
   Therefore, when writing the symbols, we additionally store an index
   value in the st_other field in bits 1-7 (bit 0 is for the _ hack, see
   above).  This index is used during disassembly to reorder all symbols
   regardless of whether they were local or global. *)

let add_elf_symbol symtable elf_sym name =
  (* looks through previously entered symbols and sets the index of
     this symbol to be one more than any previous symbols at the
     same address. *)
  let get_idx v =
    let rec get_idx' l idx =
      match l with
	[] -> idx
      |	({ st_value = v';
	   st_type = t';
	   st_other = o' }::t) -> 
	 let idx = 
	   let idx' = o' lsr 1 in
	   if v' = v then
	     if idx' >= idx then (idx'+1) else idx
	   else
	     idx in
	 get_idx' t idx in
    get_idx' symtable.st_global_symbols 
      (get_idx' symtable.st_local_symbols 0) in

  (* add indexing field to help disassembler *)
  if (* (elf_sym.st_type = stt_notype) || *)
     (elf_sym.st_type = stt_object) ||
     (elf_sym.st_type = stt_func) then
    elf_sym.st_other <- 
       ((get_idx elf_sym.st_value) lsl 1) lor elf_sym.st_other;
(*
  Printf.printf "adding %s symbol %s(#%d)=%x with other = %d\n"
    (if elf_sym.st_binding = stb_local then "local" else "global")
    name elf_sym.st_name elf_sym.st_value elf_sym.st_other;
*)
  let r_symbol_number = ref 0 in
  if elf_sym.st_binding = stb_local then
    begin
      symtable.st_local_symbols <- (elf_sym :: symtable.st_local_symbols);
      r_symbol_number := (List.length symtable.st_local_symbols) - 1;
    end
  else if elf_sym.st_binding = stb_global then
    begin
      symtable.st_global_symbols <- (elf_sym::symtable.st_global_symbols);
      r_symbol_number := symtable.st_local_symcount +
           (List.length symtable.st_global_symbols) - 1;
    end;
  symtable.st_symcount <- (symtable.st_symcount + 1);
  !r_symbol_number
;;

(* add_objfile_symbol
   Side effects:
     - Translates the objfile symbol into an ELF symbol and adds it to the
       symbol table.
     - Sets the old symbol's index to its new index in the symbol table. 
     - Adds the symbol's name to the string table. *)

let add_objfile_symbol strtable symtable sym =
   sym.sym_index <- (add_elf_symbol symtable 
		       (to_elf_symbol strtable sym) 
		       (id_to_string sym.sym_ident))
;;
	 
(* add_filename_symbol
   Side effects: 
    - Adds the special filename symbol to the symbol table. 
    - Adds the file name to the string table. *)

let add_filename_symbol symtable strtable srcfilename =
   let filename_symbol = {
      st_name = (add_string strtable srcfilename);
      st_value = 0;
      st_size = 0;
      st_binding = stb_local;
      st_type = stt_file;
      st_other = 0;
      st_shndx = shn_abs
    } in
   (add_elf_symbol symtable filename_symbol srcfilename)
;;
     
(* add_section_start_symbol
   Side effects: adds a section's start symbol to the symbol table. *)

let add_section_start_symbol symtable strtable sec_num sec =
   let section_symbol = {
      st_name = (add_string strtable  "");(**)(*Section symbols have no name?*)
      st_value = 0;
      st_size = 0;
      st_binding = stb_local;
      st_type = stt_section;
      st_other = 0;
      st_shndx = sec_num
    } in
   (add_elf_symbol symtable section_symbol "(section)")
;;

(* add_section_symbols
   Side effects: 
     - Adds the section start symbol and all of the symbols
       for the section into the symbol table. 
     - Puts each symbol's name into the string table. *)

let rec add_section_symbols symtable strtable sec_num secs =
   match secs with
      [] -> ()
    | (sec :: tail) ->
	 (add_section_start_symbol symtable strtable sec_num sec);
	 (List.iter (add_objfile_symbol strtable symtable) sec.s_symbols);
	 (add_section_symbols symtable strtable (sec_num + 1) tail)
;;


(* count_local_syms
   Returns the number of local symbols in the file, not including the
   filename symbol or the zero symbol. *)

let rec count_local_syms secl = 
   let rec list_sum f l = 
      match l with
	 [] -> 0
       | (i :: tail) -> (f i) + (list_sum f tail) in
   let count_1_symbol sym = 
      match sym.sym_scope with
	 Local -> 1
       | Weak -> 0
       | Global -> 0 in
   match secl with
      [] -> 0
    | (sec :: tail) -> 1 + (list_sum count_1_symbol sec.s_symbols)
	    + (count_local_syms tail)
;;			    (* 1 for section, + 1 for each local symbol. *)


	 
(* make_elf_symbol_table
   Returns an ELF symbol table for the given objfile, with all ELF-specific
     and normal symbols added.
   Side effects: Each objfile symbol in the symbol dictionary is given its
     actual index in the symbol list. *)

let make_elf_symbol_table strtable srcfilename objfile = 
   let symtable = empty_elf_symbol_table() in
   symtable.st_local_symcount <- (count_local_syms objfile.o_secs) + 2;
   (add_filename_symbol symtable strtable srcfilename);
   (add_section_symbols symtable strtable 1 objfile.o_secs);
   (List.iter (add_objfile_symbol strtable symtable) objfile.o_import_symbols);
   symtable
;;

(* make_symtable_section
   Returns an ELF section for the given symbol table. 
   Side effects: The section's name is put in secnames. *)

let make_symtable_section secnames strtable_section symtable = {
   es_number = 0;			(* Determined at write time *)
   es_header = { sh_name = (add_string secnames sname_symtab);
		 sh_type = sht_symtab;
		 sh_flags = 0;
		 sh_addr = 0;
		 sh_offset = 0;		(* Determined at write time *)
		 sh_size = (symtable.st_symcount * sizeof_elf_symbol);
		 sh_link = Section_link strtable_section;
		 sh_info = Int_link symtable.st_local_symcount;
		 sh_addralign = 1;
		 sh_entsize = sizeof_elf_symbol
	       };
   es_data = Symbol_data symtable
 }
;;

(* make_symbol_sections
   Returns a (string table section, symbol table section) pair for the 
   symbols in the object file. 
   Side effects: the sections' names are put in the section names table. *)

let make_symbol_sections secnames srcfilename objfile = 
   let strtable = empty_string_table() in
   let symtable = (make_elf_symbol_table strtable srcfilename objfile) in
   
   let strtable_section = (make_strtable_section secnames strtable) in
   let symtable_section = (make_symtable_section secnames strtable_section
				symtable) in
   (strtable_section, symtable_section)
;;

(***** Program section functions *****)

let flags_for_section sec = 
   if sec.s_name = ".data" then
      (shf_alloc + shf_write)
   else
      (shf_alloc + shf_execinstr)
;;

(* make_program_section
   Returns a new section and section header for a program data section
     (text or data). The section's name, data, and size are taken from the
     objfile section, but no offset or section number is given. 
     These values will be assigned when the section is written. 
   Side effects: the section's name is added to the section name 
     string table. *)

let make_program_section secnames sec = {
   es_number = 0;			(* assigned when written *)
   es_header =   { sh_name = (add_string secnames sec.s_name);
		   sh_type = sht_progbits;
		   sh_flags = (flags_for_section sec);
		   sh_addr = 0;
		   sh_offset = 0;	(* assigned when written *)
		   sh_size = (Stringchan.get_mark sec.s_data);
		   sh_link = Int_link 0;
		   sh_info = Int_link 0;
		   sh_addralign = 4;
		   sh_entsize = 0
		 };
   es_data = Prog_data sec.s_data
 }
;;

(***** Relocation table functions *****)

let elf_reloctype_none = 0
and elf_reloctype_dir32 = 1
and elf_reloctype_rel32 = 2
;;


(* write_reloc_addend
   Side effects: If the relocation has an addend, writes it as an implicit
   addend in the code. Subtracts 4 from the addend to compensate for Intel's
   jump instructions which go from PC+4. *)

let write_reloc_addend secdata reloc = 
   let addend = 
      (if reloc.rel_relativity = Relative then
	 reloc.rel_addend -$ i32_4
      else
	 reloc.rel_addend) in
   let savemark = (Stringchan.get_mark secdata) in
   (Stringchan.set_mark secdata reloc.rel_pos);
   begin
      match reloc.rel_scale with
	 Byte2 ->
	    let (b1,b2) = int32_to_2bytes addend in
	    Stringchan.put_char secdata b1;
	    Stringchan.put_char secdata b2
       | Byte4 ->
	    let (b1,b2,b3,b4) = int32_to_4bytes addend in
	    Stringchan.put_char secdata b1;
	    Stringchan.put_char secdata b2;
	    Stringchan.put_char secdata b3;
	    Stringchan.put_char secdata b4
       | _ -> failwith "write_reloc_addend @ coff.ml: reloc scale"
   end;
   (Stringchan.set_mark secdata savemark)
;;


(* elf_reloc_type_of
   Returns the ELF relocation type for the given relocation relativity
   and scale. *)

let elf_reloctype_of relativity scale = 
   match (relativity, scale) with
      (Relative, Byte4) -> elf_reloctype_rel32
    | (Absolute, Byte4) -> elf_reloctype_dir32
    | _ -> failwith "elf_reloctype_of @ elf.ml : bad relocation type."
;;

(* to_elf_relocation
   Returns a new ELF relocation for the given objfile relocation. *)

let to_elf_relocation sec reloc = 
   write_reloc_addend sec.s_data reloc;	(* Write the reloc addend - 4 *)
 {
   r_offset = reloc.rel_pos;
   r_sym = reloc.rel_symbol.sym_index;
   r_type = (elf_reloctype_of reloc.rel_relativity reloc.rel_scale)
 }
;;

(* make_relocation_section
   Returns a new relocation section for the given program section.
   Symbol_section is the symbol table, program_section is the 
   section in the ELF file, and objfile_section is the same section from
   the objfile. The relocations from the objfile section are translated
   into ELF relocations. 
   Side effects: the section's name is added to the section names table. *)

let make_relocation_section secnames symbol_section
      program_section objfile_section = 
 {
   es_number = 0;			(* assigned when written *)
   es_header = { sh_name = (add_string secnames 
			     (sname_rel objfile_section.s_name));
		 sh_type = sht_rel;
		 sh_flags = 0;
		 sh_addr = 0;
		 sh_offset = 0;		(* assigned when written *)
		 sh_size = ((List.length objfile_section.s_relocs) 
				 * sizeof_elf_reloc);
		 sh_link = Section_link symbol_section;
		 sh_info = Section_link program_section;
		 sh_addralign = 4;
		 sh_entsize = sizeof_elf_reloc
	       };
   es_data = Relocation_data (List.map (to_elf_relocation objfile_section)
				  objfile_section.s_relocs)
 }
;;

(***** Etc. *****)

(* make_secnames_section
   Returns a new section for the section names table.
   Side effects: The name of the section names table section is added to the
     table before the section is made. *)
	    
let make_secnames_section secnames = 
   let sec_name = (add_string secnames sname_shstrtab) in 
   { es_number = 0;
      es_header = { sh_name = sec_name;
		    sh_type = sht_strtab;
		    sh_flags = 0;
		    sh_addr = 0;
		    sh_offset = 0;		(* assigned when written *)
		    sh_size = secnames.st_length;
		    sh_link = Int_link 0;
		    sh_info = Int_link 0;
		    sh_addralign = 1;
		    sh_entsize = 0
		  };
      es_data = String_data secnames
    }
;;

(***************************** WRITING ****************************)

(* put_link
   Writes an Int_link or a Section_link to the file. 
   An Int_link is just written; a Section_link is resolved by looking up
   the section's number and writing that. *)

let put_link outfile link = 
   match link with
      Int_link i -> put_word outfile i
    | Section_link s -> put_word outfile s.es_number
;;

(* write_reloc
   Side effects: writes a single ELF relocation. *)

let write_reloc outfile reloc = 
   begin
      put_word outfile reloc.r_offset;
      put_word outfile ((reloc.r_sym lsl 8) lor (reloc.r_type land 0xFF));
   end
;;

(* write_symbol
   Side effects: writes a single ELF symbol. *)

let write_symbol outfile symbol =
   begin
      put_word outfile symbol.st_name;
      put_word outfile symbol.st_value;
      put_word outfile symbol.st_size;
      put_byte outfile ((symbol.st_binding lsl 4) 
			     lor (symbol.st_type land 0xF));
      put_byte outfile symbol.st_other;
      put_hword outfile symbol.st_shndx;
   end
;;

(* write_symbol_table
   Side effects: writes all symbols in the symbol table. *)

let write_symbol_table outfile symtable = 
   begin
      (List.iter (write_symbol outfile) (List.rev symtable.st_local_symbols));
      (List.iter (write_symbol outfile) (List.rev symtable.st_global_symbols));
   end
;;

(* write_string_table
   Side effects: writes all strings in the string table. *)

let write_string_table outfile strtable = 
   let rec write_strings outfile strs =
      match strs with 
	 [] -> ()
       | (str :: tail) -> 
	    (write_strings outfile tail);(* Write strings in reverse order *)
	    (put_string outfile str);	(* Output string *)
	    (put_byte outfile 0);	(* Follow string with \0 byte *)
   in
   (put_byte outfile 0);		(* Write initial \0 byte for table. *)
   (write_strings outfile strtable.st_strings)
;;

(* write_section_data
   Side effects: Writes the data for any type of ELF section. *)

let write_section_data outfile sec sec_number = 
   begin
      sec.es_header.sh_offset <- (pos_out outfile); (* Record actual position *)
      sec.es_number <- sec_number;	(* Record actual section number *)
	    
      begin
	 match sec.es_data with
	  | Prog_data secdata -> 
	       (output outfile (Stringchan.to_string secdata) 0 
		     (Stringchan.get_mark secdata))
	  | Relocation_data relocs ->
	       (List.iter (write_reloc outfile) relocs)
	  | Symbol_data symbols ->
	       (write_symbol_table outfile symbols)
	  | String_data strings ->
	       (write_string_table outfile strings)
	  | No_data -> ()
      end;

      (* Pad to next section boundary boundary. *)
      (sec_pad outfile);
   end
;;

(* write_sections_data
   Side effects: writes the data for a list of sections. *)

let rec write_sections_data outfile secs secnum = 
   match secs with
      [] -> ()
    | (sec :: tail) ->
	 (write_section_data outfile sec secnum);
	 (write_sections_data outfile tail (secnum + 1))
;;

(* write_section_header
   Side effects: writes the header for a section. *)

let write_section_header outfile sec =
   begin
      let sh = sec.es_header in
      put_word outfile sh.sh_name;
      put_word outfile sh.sh_type;
      put_word outfile sh.sh_flags;
      put_word outfile sh.sh_addr;
      put_word outfile sh.sh_offset;
      put_word outfile sh.sh_size;
      put_link outfile sh.sh_link;
      put_link outfile sh.sh_info;
      put_word outfile sh.sh_addralign;
      put_word outfile sh.sh_entsize;
   end
;;

(* write_elf_file_header
   side effects: writes the ELF file header to the file. *)

let write_elf_file_header outfile h = 
   begin
      let header_start_pos = (pos_out outfile) in
      put_string outfile h.ei_magic;
      put_byte outfile h.ei_class;
      put_byte outfile h.ei_data;
      put_byte outfile h.ei_version;
      
      seek_out outfile (header_start_pos + 16);(* Skip to 0+16 bytes*)
      put_hword outfile h.e_type;
      put_hword outfile h.e_machine;
      put_word outfile h.e_version;
      put_word outfile h.e_entry;
      put_word outfile h.e_phoff;
      put_word outfile h.e_shoff;
      put_long outfile h.e_flags;
      put_hword outfile h.e_ehsize;
      put_hword outfile h.e_phentsize;
      put_hword outfile h.e_phnum;
      put_hword outfile h.e_shentsize;
      put_hword outfile h.e_shnum;
      put_hword outfile h.e_shstrndx;
   end
;;

(* write_elf 
   Side effects: Creates an ELF file from the given objfile, 
   and writes it into an open file handle. *)

let write_elf outfile srcfilename objfile  = 
   let secnames = empty_string_table() in

   let (symbol_string_section, symbol_section) = 
      (make_symbol_sections secnames srcfilename objfile) in

   let program_sections = 
      (List.map (make_program_section secnames) objfile.o_secs) in

   let relocation_sections = 
      (List.map2 (make_relocation_section secnames symbol_section) 
	    program_sections objfile.o_secs) in

   let section_name_string_section = (make_secnames_section secnames) in

   (* Put the sections in a list. *)
   let sections = (make_zero_section() :: 
		     (program_sections @ relocation_sections @ 
			[symbol_section; symbol_string_section; 
			   section_name_string_section])) in
   begin
      (* Now start writing. Update the section headers with the 
         correct offset, size, and section number as we go. *)
   
      (* Skip the file header; we'll fill it in later. *)
      (seek_out outfile sizeof_elf_file_header);
   
      (* Write section data and update the section headers. *)
      (write_sections_data outfile sections 0);

      (* Now write the section headers. *)
      let section_header_table_pos = (pos_out outfile) in
      (List.iter (write_section_header outfile) sections);

      (* Now seek back and write the file header. *)
      (seek_out outfile 0);
      (write_elf_file_header outfile 
	    (make_elf_file_header section_header_table_pos 
		  (List.length sections)
		  section_name_string_section.es_number));
   end
;;

(* create_elf
   Side effects: Creates and writes a new ELF file from the given objfile. *)
   
let create_elf srcfilename outfilename objfile = 
   let outfile = (open_out outfilename) in
   (write_elf outfile srcfilename objfile);
   (close_out outfile)
;;

(* EOF: elf.cmo *)
