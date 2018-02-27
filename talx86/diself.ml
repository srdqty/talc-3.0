(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* diself.ml
 *
 * Provides interface objfile implementation for reading ELF object
 * files.
 *)
open Numtypes;;
open Disobjfile;;
open Identifier;;

module type diself = sig
 val elf_objfile : string ->  Disobjfile.objfile
end

module DiselfFunc (
X : sig
   type in_channel
   val input_byte : in_channel -> int
   val pos_in : in_channel -> int
   val seek_in : in_channel -> int -> unit
   val really_input : in_channel -> string -> int -> int -> unit
   val open_in_bin : string -> in_channel
   val close_in : in_channel -> unit
end ) : diself = struct

type in_channel = X.in_channel
open X

module Binin = Binin.BininFunc (X)

open Binin



(********************************** TYPES ************************************)

(* Int32's are used here for fields like flags, or addresses, where all 32
   bits are potentially important. For numbers quantifying things or for
   file pointers, ints are used instead (since the OCaml functions can't
   use int32's anyway). *)

(** Headers **)

type elf_string_table = string

type sh_string_table =
    Index of int
  | Table of elf_string_table

type elf_file_header = {
   (* The following make up e_ident *)
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
   e_flags : int;			(* word: flags *)
   e_ehsize : int;			(* hword: ELF header size *)
   e_phentsize : int;			(* hword: prog header entry size *)
   e_phnum : int;			(* hword: # of prog header entries *)
   e_shentsize : int;			(* hword: sec header entry size *)
   e_shnum : int;			(* hword: # of sec header entries *)
   e_shstrndx : int                     (* hword: sec name str table index *)
 }
;;

let i386_magic_number = "\127ELF";;
let i386_class = 1;;                    (* ELFCLASS32 *)
let i386_data = 1;;                     (* ELFDATA2LSB == big endian *)
let i386_version = 1;;                  (* EV_CURRENT *)
exception Bad_Magic_Number of string;;

(***** Symbol table *****)

type elf_symbol = {
   st_name : string;			(* name of the symbol *)
   st_value : int;			(* word: offset of symbol in section *)
   st_size : int;			(* word: size *)
   st_binding : int;			(* 4bits: stb_* *)
   st_type : int;			(* 4bits: stt_* *)
   st_other : int;			(* byte, reserved. *)
   st_shndx : int			(* hword: section num. of symbol *)
 }
;;

type elf_symbol_table = elf_symbol array

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
    sh_name : string;
    sh_type : int;
    sh_flags : int;
    sh_addr : int;
    sh_offset : int;
    sh_size : int;
    mutable sh_link : elf_section_link;
    mutable sh_info : elf_section_link;
    sh_addralign : int;
    sh_entsize : int
 }

(**** Sections *****)

and elf_section_data =
 | Prog_data of in_channel * int (* channel + abs offset *)
 | Relocation_data of elf_relocation_entry list
 | Symbol_data of elf_symbol_table
 | String_data of string
 | No_data

and elf_section = {
   es_header : elf_section_header;
   mutable es_data : elf_section_data
 }
;;

(* Section header types *)
let sht_progbits = 1
let sht_symtab = 2
let sht_strtab = 3
let sht_rela = 4
let sht_hash = 5
let sht_dynamic = 6
let sht_rel = 9
let sht_dynsym = 11

(***** String tables *****)

(* Reads in the contents of a string table.  Assumes that the file
   position is at the beginning of the table. *)

let get_string_table n ch =
  let strtab = String.create n in
  really_input ch strtab 0 n;
  strtab

(* Looks up a string in the string table at the given offset; note
   that offset zero should always be NULL *)

exception EndOfTab
let lookup_string strtab off =
  let len = String.length strtab in
  let rec find_null i =
    if i = len then
      raise EndOfTab
    else
      if strtab.[i] = '\000' then
	i
      else find_null (i+1) in
  try
    let nullpos = find_null off in
    String.sub strtab off (nullpos-off)
  with EndOfTab ->
    failwith "Ill-formed string table or offset"


(***** Headers *****)

let print_header
    { ei_magic = ei_magic;
      ei_class = ei_class;
      ei_data = ei_data;
      ei_version = ei_version;
      e_type = e_type;
      e_machine = e_machine;
      e_version = e_version;
      e_entry = e_entry;
      e_phoff = e_phoff;
      e_shoff = e_shoff;
      e_flags = e_flags;
      e_ehsize = e_ehsize;
      e_phentsize = e_phentsize;
      e_phnum = e_phnum;
      e_shentsize = e_shentsize;
      e_shnum = e_shnum;
      e_shstrndx = e_shstrndx;
    } =
  Printf.printf "Elf header:\n  type=%d mach=%d version=%d entry=%d phoff=%d\n"
    e_type e_machine e_version e_entry e_phoff;
  Printf.printf "  shoff=%d flags=%d ehsz=%d pesize=%d pnum=%d shsize=%d\n"
    e_shoff e_flags e_ehsize e_phentsize e_phnum e_shentsize;
  Printf.printf "  shnum=%d shstrndx=%d\n"
    e_shnum e_shstrndx
;;

(* get_header
   Reads a ELF file's header and returns it as a elf_file_header.
   Preconditions: must be at beginning of file. *)

let get_header ch =
  (* get e_ident *)
  let ei_magic =
    let buf = String.create 4 in
    really_input ch buf 0 4;
    buf in
  let ei_class = get_byte ch in
  let ei_data = get_byte ch in
  let ei_version = get_byte ch in
  seek_in ch 16; (* skip padding *)
  let e_type = get_short ch in
  let e_machine = get_short ch in
  let e_version = get_int ch in
  let e_entry = get_int ch in
  let e_phoff = get_int ch in
  let e_shoff = get_int ch in
  let e_flags = get_int ch in
  let e_ehsize = get_short ch in
  let e_phentsize = get_short ch in
  let e_phnum = get_short ch in
  let e_shentsize = get_short ch in
  let e_shnum = get_short ch in
  let e_shstrndx = get_short ch in

  (* verify e_ident *)
  if (ei_magic = i386_magic_number) &&
     (ei_class = i386_class) &&
     (ei_data = i386_data) &&
     (ei_version = i386_version) then

    { ei_magic = ei_magic;
      ei_class = ei_class;
      ei_data = ei_data;
      ei_version = ei_version;
      e_type = e_type;
      e_machine = e_machine;
      e_version = e_version;
      e_entry = e_entry;
      e_phoff = e_phoff;
      e_shoff = e_shoff;
      e_flags = e_flags;
      e_ehsize = e_ehsize;
      e_phentsize = e_phentsize;
      e_phnum = e_phnum;
      e_shentsize = e_shentsize;
      e_shnum = e_shnum;
      e_shstrndx = e_shstrndx;
    }
  else raise (Bad_Magic_Number ei_magic);;

(***** Sections *****)


(* Reads an elf section header and returns it.  Assumes the file
   marker is correctly positioned.  If strtab_opt is None, then we are
   reading in the section header strtab. *)

let get_section_header ch strtab_opt =
  let sh_name =
    let name_idx = get_int ch in
    match strtab_opt with
      None -> ".shstrtab"
    | Some strtab ->
	lookup_string strtab name_idx in
  let sh_type = get_int ch in
  let sh_flags = get_int ch in
  let sh_addr = get_int ch in
  let sh_offset = get_int ch in
  let sh_size = get_int ch in
  let sh_link = Int_link (get_int ch) in
  let sh_info = Int_link (get_int ch) in
  let sh_addralign = get_int ch in
  let sh_entsize = get_int ch in
  { sh_name = sh_name;
    sh_type = sh_type;
    sh_flags = sh_flags;
    sh_addr = sh_addr;
    sh_offset = sh_offset;
    sh_size = sh_size;
    sh_link = sh_link;
    sh_info = sh_info;
    sh_addralign = sh_addralign;
    sh_entsize = sh_entsize;
  }
;;

let print_section_header
    { sh_name = sh_name;
      sh_type = sh_type;
      sh_flags = sh_flags;
      sh_addr = sh_addr;
      sh_offset = sh_offset;
      sh_size = sh_size;
      sh_link = sh_link;
      sh_info = sh_info;
      sh_addralign = sh_addralign;
      sh_entsize = sh_entsize;
    } =
  Printf.printf "Section header:\n  name=%s type=%d flags=%d addr=%d\n"
    sh_name sh_type sh_flags sh_addr;
  Printf.printf "  offset=%d size=%d link=%d info=%d align=%d esize=%d\n"
    sh_offset sh_size
    (match sh_link with Int_link i -> i | _ -> -1)
    (match sh_info with Int_link i -> i | _ -> -1)
    sh_addralign sh_entsize
;;

(* Reads the section headers from the file, returning as an array of
   section_header's *)

let get_sections h ch =
  (* First get Section Header string table *)
  let sh_strtab_hdr =
    seek_in ch (h.e_shoff + h.e_shstrndx * h.e_shentsize);
    get_section_header ch None in
(*  let _ = print_header h in
  let _ = print_section_header sh_strtab_hdr in *)
  let sh_strtab =
    if (sh_strtab_hdr.sh_type = sht_strtab) &&
       (sh_strtab_hdr.sh_flags = 0) then begin
	seek_in ch sh_strtab_hdr.sh_offset;
	get_string_table sh_strtab_hdr.sh_size ch end
    else
      failwith
	(Printf.sprintf
	   "Reading Section String table: invalid header type %d"
	   sh_strtab_hdr.sh_type)
 in

  (* Reads in section header #i; does data later *)
  let init_section i =
    if i = h.e_shstrndx then (* don't read it again *)
      { es_header = sh_strtab_hdr;
	es_data = String_data sh_strtab }
    else begin
      seek_in ch (h.e_shoff + h.e_shentsize * i);
      let hdr = get_section_header ch (Some sh_strtab) in
      (* print_section_header hdr; *)
      { es_header = hdr;
	es_data = No_data; (* temporary *) } end
  in

  (* Read in all sections *)
  Array.init h.e_shnum init_section

;;

(***** Relocations *****)

(* Prints a list of relocations *)

let print_relocations rl symbols =
  List.iter
    (fun r ->
      Printf.printf "reloc: off = %d, sym = %d (%s), type = %d\n"
	r.r_offset r.r_sym symbols.(r.r_sym).st_name r.r_type) rl

(* Reads the ELF relocation at the file marker and returns it. *)

let get_relocation sh ch =
  let r_offset = get_int ch in
  let(r_sym,r_type) =
    let r_info = get_int ch in
    (r_info lsr 8,r_info land 0xff) in
  { r_offset = r_offset;
    r_sym = r_sym;
    r_type = r_type }
;;

(* 1) reads in relocation records for relocation sections and stores them
   in the list, and 2) fixes up section_link fields in the list to point
   to these sections.

   relocations are section-relative.  Here we only expect one text
   section, so it doesn't really matter, but I'm not sure how I'd
   make it independent of that fact. *)

let get_relocations sections ch =
  (* First read in all of the relocation sections *)
  let sect_nums = ref [] in
  Array.iteri
    (fun i s ->
      if s.es_header.sh_type = sht_rel then begin
	let num_entries = s.es_header.sh_size / s.es_header.sh_entsize in
	seek_in ch s.es_header.sh_offset;
	let reloc_a =
	  Array.init num_entries
	    (fun _ -> get_relocation s.es_header ch) in
	Sort.array (fun a b -> a.r_offset <= b.r_offset) reloc_a;
	let reloc_l = Array.to_list reloc_a in
	s.es_data <- Relocation_data reloc_l;
	sect_nums :=
	   (match s.es_header.sh_info with
	     Int_link idx -> (idx,i) :: !sect_nums
	   | _ -> failwith "Found non-Intlink in relocation recs") end
      else if s.es_header.sh_type = sht_rela then
	failwith "Don't support addend-style relocation records"
      else ()) sections;

  (* Now separate relocation information by section *)
  Array.init (Array.length sections)
    (function i ->
      try
	let r_idx = List.assoc i !sect_nums in
	(match sections.(r_idx).es_data with
	  Relocation_data rl -> rl
	|	_ -> failwith "internal error: malformed index in relocations")
      with Not_found -> [])
;;

(***** Symbols *****)

let stt_undefined = 0
let stt_object = 1
let stt_func = 2
let stt_section = 3
let stt_file = 4
let shn_abs = 0xfff1
let shn_undef = 0
let shn_common = 0xfff2

(* Reads in an elf symbol.  Assumes the file marker is well-positioned. *)

let get_elf_symbol ch strtab sections =
  let st_name =
    let st_offset = get_int ch in
    lookup_string strtab st_offset in
  let st_value = get_int ch in
  let st_size = get_int ch in
  let (st_binding,st_type) =
    let st_bindtype = get_byte ch in
    (st_bindtype lsr 4,st_bindtype land 0xf) in
  let st_other = get_byte ch in
  let st_shndx = get_short ch in

  let st_name' =

    (* Hack #1 -- it looks like the section labels in the ELF files are
       not present (empty strings).  Therefore, fill in the name based on
       the section in this case. *)

    if (st_name = "") &&
       (st_type = stt_section) then
      sections.(st_shndx).es_header.sh_name

    (* Hack #2 -- undo _ removal; normally st_other has a 0, so if
       it has a 1, we know that our ELF assembler stuck it in there
       to say that the leading underscore was removed. *)

    else if (st_type = stt_object ||
             st_type = stt_func ||
	     st_type = stt_undefined) && ((st_other land 1) = 1) then
      "_"^st_name
    else
      st_name in

  { st_name = st_name';
    st_value = st_value;
    st_size = st_size;
    st_binding = st_binding;
    st_type = st_type;
    st_other = st_other;
    st_shndx = st_shndx;
  }
;;

(* Returns an array consisting of the symbols in the file. *)

let get_symbol_table sections ch =
  (* Find the symbol table header in the section headers *)
  let symtab_section =
    let rec find_symtab_section i len =
      if i = len then
	failwith "get_symbol_table: Could not find a symbol table!"
      else
	if sections.(i).es_header.sh_type = sht_symtab then
	  sections.(i)
	else
	  find_symtab_section (i+1) len in
    find_symtab_section 0 (Array.length sections) in

  (* Get the string table *)
  let strtab =
    (match (symtab_section.es_header.sh_link) with
      Int_link idx ->
	(let hdr = sections.(idx).es_header in
	let tab =
	  if (hdr.sh_type = sht_strtab) &&
	    (hdr.sh_flags = 0) then begin
	      seek_in ch hdr.sh_offset;
	      get_string_table hdr.sh_size ch end
	  else
	    failwith "Reading String table: invalid header type" in
	symtab_section.es_header.sh_link <-
	   Section_link { es_header = hdr;
			  es_data = String_data tab };
	tab)
    | _ -> failwith "Invalid section data in symtab section") in

  (* Read in the symbol table and return it *)
  let num_entries = symtab_section.es_header.sh_size /
    symtab_section.es_header.sh_entsize in
  seek_in ch symtab_section.es_header.sh_offset;
  Array.init num_entries (function _ -> get_elf_symbol ch strtab sections)
;;

(* split_symbols : int -> elf_symbol array ->
 *                  (int,identifier list) dict array
 *
 * Given all of the symbols we produce an array (indexed by section) of
 * dictionaries (indexed by relative position) of lists of symbols
 * (the list should have all symbols at the same position).  This
 * allows us to determine whether a virtual address within a section
 * has a label associated with it.  Note that multiple labels may
 * be associated with an address.
 *
 * MWH 10/30/99 Assuming the labels in the symbol table are the same
 * order as in the .tal file is not, in general, correct --- ELF files
 * should divide symbols in the table into local and global symbols,
 * with all global symbols appearing first and local symbols appearing
 * afterwards.  To validate the assumption made here, we modified
 * elf.ml:add_elf_symbol to store additional information in the
 * st_other field: bits 1-7 are used to indicate the order that the
 * symbol was entered into the table whether global or local.  Thus,
 * after gathering all symbols at the same address, we sort them in
 * reverse order of their indicated index to restore the order in the
 * original .tal file.
 *)

let split_symbols num_secs symbols =
  (* arrange the symbols by section number *)
  let sec_syms = Array.create num_secs [] in
  Array.iteri
    (fun i s ->
      let sec = s.st_shndx in
      if sec >= 0 && sec < num_secs then
	sec_syms.(sec) <- s::sec_syms.(sec))
    symbols;

  (* Sort the symbols in each section *)
  let comp_sym s1 s2 = s1.st_value <= s2.st_value in
  Array.iteri (fun i s -> sec_syms.(i) <- Sort.list comp_sym s) sec_syms;

  (* merge symbols that are the same into a list, preserving order *)
  let sym2id s = id_of_string s.st_name in
  (* sort symbols found to be the same based on their idx stored
     in the st_other field *)
  let rec merge dict syms =
    match syms with
      [] -> dict
    | s1::rest ->
	let rec split lis =
	  match lis with
	    [] -> ([],[])
	  | s2::rest ->
	      if s2.st_value = s1.st_value then
		let (same,others) = split rest in
		(s2::same,others)
	      else ([],lis) in
	(* get all symbols at the same address *)
	let (same'',others) = split rest in
	(* sort the list based on the index portion of the st_other field *)
	let same' = Sort.list (* reverse order *)
	    (fun s1 s2 ->
	      (s1.st_other lsr 1) >= (s2.st_other lsr 1))
	    (s1::same'') in
	(* map the list into just the identifiers *)
	let same = List.map (fun x -> sym2id x) same' in
	merge (Dict.insert dict s1.st_value same) others
  in Array.map (merge (Dict.empty compare)) sec_syms
;;

let print_symbols dict =
  Dict.app_dict
    (fun i sl -> Printf.printf "offset %d: " i;
      List.iter	(fun s -> Printf.printf "|%s| " (id_to_string s)) sl;
      Printf.printf "\n") dict
;;

(**************************************************************************)

let elf_objfile filename =
  let ch = open_in_bin filename in

  (* First read in the ELF header *)
  let header = get_header ch in

  (* Then read in the sections; this will read in the section header
     string table as a side-effect, but we won't return it here since
     it will no longer be needed *)
  let sections = get_sections header ch in

  (* Then read in the symbol table *)
  let symbols = get_symbol_table sections ch in

  (* Finally read in the relocations *)
  let relocs = get_relocations sections ch in
(*
  Printf.printf "symbol table (length = %d):\n" (Array.length symbols);
  Array.iteri
    (fun i s ->
      Printf.printf "%d: %s, val=%d, siz=%d, bin=%d, typ=%d, oth=%d, sec=%s\n"
	i s.st_name s.st_value s.st_size s.st_binding s.st_type s.st_other
	(if s.st_shndx = shn_undef then "UNDEFINED"
	else if s.st_shndx = shn_abs then "ABSOLUTE"
	else if s.st_shndx = shn_common then "COMMON"
	else if s.st_shndx < (Array.length sections) then
	  sections.(s.st_shndx).es_header.sh_name
	else "??"))
    symbols;
  for i=0 to (Array.length relocs) - 1 do
    Printf.printf "relocations for section %d\n" i;
    print_relocations relocs.(i) symbols;
  done;
*)
  let label_offset =
    Array.fold_left
      (fun d s ->
        Dict.insert d
          (id_of_string s.st_name)
          (s.st_shndx,s.st_value))
      (Dict.empty id_compare)
      symbols in
  let sec_symbols = split_symbols header.e_shnum symbols in
  let sec_shares  = Array.map offset_dict_to_virtlabels sec_symbols in
  let cur_section = ref 0 in
  (* end of section is the seek position before this *)
  let cur_section_stop = ref 0 in
  let cur_addr = ref 0 in
  let cur_relocations = ref [] in
  let cur_symbols : (int,identifier list) Dict.dict ref =
    ref (Dict.empty compare) in
  let cur_shares : (identifier,identifier list)Dict.dict ref =
    ref (Dict.empty id_compare) in

  (* seek to a particular section and ensure that the flags are set
   * appropriately. set the cur_<foo> refs. *)
  let seek_section s exn () =
    try
      begin
	cur_section := 0;
	(try
	  while sections.(!cur_section).es_header.sh_name <> s do
	    cur_section := !cur_section + 1
	  done
	with Invalid_argument "Array.get" -> raise exn);
	let cur_sec_hdr = sections.(!cur_section).es_header in
(*
	print_section_header cur_sec_hdr;
*)
	(* XXX do flag checking ? *)
	cur_addr := (cur_sec_hdr.sh_addr);
	seek_in ch cur_sec_hdr.sh_offset;
	cur_section_stop := cur_sec_hdr.sh_offset + cur_sec_hdr.sh_size;
	cur_relocations  := relocs.(!cur_section);
	cur_symbols      := sec_symbols.(!cur_section);
	cur_shares       := sec_shares.(!cur_section)
(*
	print_relocations !cur_relocations symbols;
	print_symbols !cur_symbols
*)
      end
    with _ -> raise exn in

  (* Get all of the labels associated with the position and return
   * as a list.  *)
  let current_labels offset =
    try Dict.lookup (!cur_symbols) ((!cur_addr)+offset)
    with Dict.Absent -> [] in
  let current_shares () = !cur_shares in

  (* Get the next byte in a section, raising End_of_Section if we go past
   * the end of the current section. *)
  let obj_get_byte () =
    if (pos_in ch) < (!cur_section_stop) then
      (let b = get_byte ch in
(*      Printf.printf "obj_get_byte: read %x at addr %x\n"
	b !cur_addr; *)
      cur_addr := (!cur_addr) + 1; b)
    else raise End_of_Section in

  let obj_align a =
    try
      while ((!cur_addr) mod a) <> 0 do
	if (obj_get_byte()) <> 0 then
	  failwith "obj_align:  non-zero byte!";
      done
    with End_of_Section -> () in

  (* Return a relocation identifier (if any) for the current address.
   * As a side-effect, remove this relocation from the current list of
   * relocations. *)
  let get_reloc () =
    match !cur_relocations with
      [] -> None
    | r::rest ->
	if r.r_offset = (!cur_addr) then
	  begin
	    cur_relocations := rest;
(*	    Printf.printf "get_reloc(): addr = %x, reloc_sym = %s\n"
	      r.r_offset symbols.(r.r_sym).st_name; *)
	    Some(id_of_string(symbols.(r.r_sym).st_name))
	  end
	else None in

  let get_pos () = (!cur_section,!cur_addr) in

  { obj_close = (fun () -> close_in ch);
    obj_seek_code         = seek_section ".text" No_Code_Section;
    obj_seek_data         = seek_section ".data" No_Data_Section;
    obj_seek_cyclone      = seek_section ".cyc" No_Data_Section;
    obj_get_labels        = current_labels;
    obj_get_shared_labels = current_shares;
    obj_get_byte          = obj_get_byte;
    obj_align             = obj_align;
    obj_get_reloc         = get_reloc;
    obj_get_pos           = get_pos;
    obj_label_offset      = label_offset
  }
;;
end
