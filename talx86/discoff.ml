(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* discoff.ml
 *
 * Provides interface objfile implementation for reading COFF object
 * files.
 *)
open Numtypes;;
open Disobjfile;;
open Identifier;;

module type discoff = sig
   val coff_objfile : string -> Disobjfile.objfile
end

module DiscoffFunc (
X : sig
   type in_channel
   val input_byte : in_channel -> int
   val seek_in : in_channel -> int -> unit
   val really_input : in_channel -> string -> int -> int -> unit
   val pos_in : in_channel -> int
   val open_in_bin : string -> in_channel
   val close_in : in_channel -> unit
end ) : discoff = struct

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

type coff_file_header = {
    h_magic : int;
    h_num_sections : int;
    h_time_date : int32;
    h_sym_table_offset : int;
    h_sym_table_entries : int;
    h_optional_header_size : int;
    h_flags : int
  }
;;

(* Keep these in sync with coff.ml *)
let i386_magic_number = 0x14C;;
let shflags_textsection = (0x6030, 0x0020)(* Code, 4-byte align, -rx *)
and shflags_datasection = (0xC030, 0x0040)(* Data, 4-byte align, wr- *)
and shflags_cyclonesection = (0x6030, 0x0060)

exception Bad_Magic_Number of int;;


type coff_section_header = {
    sh_section_name : string;  (* 8 chars -- null pad *)
    sh_physical_address : int32;
    sh_virtual_address : int32;
    sh_section_size : int;
    sh_section_offset : int;
    sh_relocation_offset : int;
    sh_line_number_offset : int;
    sh_num_relocation_entries : int;
    sh_num_line_number_entries : int;
    sh_flags : int * int;
  }
;;

let bogus_section_header = {
   sh_section_name = "";
    sh_physical_address = i32_0;
    sh_virtual_address = i32_0;
    sh_section_size = 0;
    sh_section_offset = 0;
    sh_relocation_offset = 0;
    sh_line_number_offset = 0;
    sh_num_relocation_entries = 0;
    sh_num_line_number_entries = 0;
    sh_flags = (0,0)
  }
;;

(** Symbols **)

type coff_symbol = {
    coff_sym_offset : int;   (* used internally for sorting only *)
    coff_sym_name : string;
    coff_sym_value : int;
    coff_sym_section : int;
    coff_sym_type : int;
    coff_sym_class : int;
    coff_sym_num_aux : int	(* NOTE difference: num_aux instead of aux. *)
 }
;;

let bogus_symbol = {
   coff_sym_offset = 0;
   coff_sym_name = "bogus";
   coff_sym_value = 0;
   coff_sym_section = 0;
   coff_sym_type = 0;
   coff_sym_class = 0;
   coff_sym_num_aux = 0
 }
;;


let symtype_null = 0
and symclass_external = 2
and symclass_static = 3
and symclass_weakext = 108
and symsection_undef = 0
;;


(** Relocations **)

type coff_relocation = {
   coff_rel_pos : int;			(* Virtual position *)
   coff_rel_symindex : int;		(* Index of symbol referred to *)
   coff_rel_type : int			(* Rel/abs, 16/32-bit *)
 }
;;

let bogus_relocation = {
   coff_rel_pos = 0;
   coff_rel_symindex = 0;
   coff_rel_type = 0
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


(***** Headers *****)

(* get_header
   Reads a COFF file's header and returns it as a coff_file_header.
   Preconditions: must be at beginning of file. *)

let get_header ch =
   let mag = get_short ch in
   let sec = get_short ch in
   let td = get_int32 ch in
   let sto = get_int ch in
   let ste = get_int ch in
   let ohs = get_short ch in
   let hf = get_short ch in
   if mag = i386_magic_number then
    { h_magic = mag;
      h_num_sections = sec;
      h_time_date = td;
      h_sym_table_offset = sto;
      h_sym_table_entries = ste;
      h_optional_header_size = ohs;
      h_flags = hf
    }
  else raise (Bad_Magic_Number mag);;


(* get_optional_header
   Returns the optional header as a string.
   Preconditions: Must be at start of optional header in file. *)

let get_optional_header header ch =
   let opt_header = String.create header.h_optional_header_size in
   really_input ch opt_header 0 header.h_optional_header_size;
   opt_header
;;

(***** String Tables *****)

(* string_table_position
   Returns: a file pointer to the position of the string table. *)

let string_table_position h =
   h.h_sym_table_offset + (sizeof_symbol * h.h_sym_table_entries)
;;

(* read_string_table
   Reads the string table and returns it as one long string.
   The first 4 bytes are kept blank to preserve index numbers. *)

let read_string_table header ch =
   let save_pos = pos_in ch in
   (seek_in ch (string_table_position header));
   let string_table_length = (get_int ch) in
   let string_table = (String.create string_table_length) in
   (really_input ch string_table 4 (string_table_length - 4));
   (seek_in ch save_pos);
   string_table
;;

(* get_string_from_table
   Looks up the string with the given index in the string table,
   and returns that string. *)

let get_string_from_table table index =
   let stringend = (String.index_from table index '\000') in
   (String.sub table index (stringend - index))
;;

(* get_coff_string
   Reads the COFF string at the file marker, and returns it as a string.
   The COFF string at the file marker can be either a string of up to 8
   characters, or a number indicating the string index in the string table. If
   the COFF string is a number, the string is retrieved from the string
   table. *)

let get_coff_string ch string_table =
   let short_string = String.create 8 in

   (* Does it begin with 0? *)
   really_input ch short_string 0 4;
   if short_string.[0] == '\000' then
      (* If so, it's a string index. *)
      (* Read string index and look up that string from the table. *)
      let index = get_int ch in
      (get_string_from_table string_table index)
   else
      begin
      (* If not, read the rest of the string and return the string. *)
	 really_input ch short_string 4 4;
	 if String.contains short_string '\000' then
	    (String.sub short_string 0 (String.index short_string '\000'))
	 else
	    short_string
      end
;;

(***** Relocations *****)

let print_relocations rl symbols =
  List.iter
    (fun r ->
	Printf.printf "reloc: off = %d, sym = %d (%s), type = %d\n"
	  r.coff_rel_pos r.coff_rel_symindex
	  symbols.(r.coff_rel_symindex).coff_sym_name
	  r.coff_rel_type) rl

(* get_relocation
   Reads the COFF relocation at the file marker and returns it. *)

let get_relocation sh ch =
   let pos = get_int ch in
   let symindex = get_int ch in
   let rtype = get_short ch in
   { coff_rel_pos = pos;
     coff_rel_symindex = symindex;
     coff_rel_type = rtype
   }
;;

(* get_relocations
   Reads the relocations for a section and returns as a list of relocations
   sorted by the position in which they are used in the code.
   Side effects: file marker is lost. *)

let get_relocations sh ch =
   (seek_in ch sh.sh_relocation_offset);
   let reloc_a = Array.init sh.sh_num_relocation_entries
	 (fun i -> (get_relocation sh ch)) in
   Sort.array (fun a b -> a.coff_rel_pos <= b.coff_rel_pos) reloc_a;
   let reloc_l = Array.to_list reloc_a in
   reloc_l
;;



(***** Sections *****)

let print_section_header {
  sh_section_name = sh_section_name;
  sh_physical_address = sh_physical_address;
  sh_virtual_address = sh_virtual_address;
  sh_section_size = sh_section_size;
  sh_section_offset = sh_section_offset;
  sh_relocation_offset = sh_relocation_offset;
  sh_line_number_offset = sh_line_number_offset;
  sh_num_relocation_entries = sh_num_relocation_entries;
  sh_num_line_number_entries = sh_num_line_number_entries;
  sh_flags = (flag1,flag2);
} =
  Printf.printf "Section Header:\n  %s pa=%d va=%d size=%d off=%d\n"
    sh_section_name (int32_to_int sh_physical_address)
    (int32_to_int sh_virtual_address) sh_section_size sh_section_offset;
  Printf.printf "  roff=%d loff=%d rnum=%d lnum=%d flags=%d,%d\n"
    sh_relocation_offset sh_line_number_offset sh_num_relocation_entries
    sh_num_line_number_entries flag1 flag2
;;

(* get_section_header
   Reads a COFF section header and returns it.
   Preconditions: The file marker must be at the start of a section header. *)

let get_section_header ch strs =
   let sec_name = get_coff_string ch strs in
   let pa = get_int32 ch in
   let va = get_int32 ch in
   let ss = get_int ch in
   let so = get_int ch in
   let ro = get_int ch in
   let lno = get_int ch in
   let nre = get_short ch in
   let nlne = get_short ch in
   let sf1 = get_short ch in
   let sf2 = get_short ch in
   begin
    { sh_section_name = sec_name;
      sh_physical_address = pa;
      sh_virtual_address = va;
      sh_section_size = ss;
      sh_section_offset = so;
      sh_relocation_offset = ro;
      sh_line_number_offset = lno;
      sh_num_relocation_entries = nre;
      sh_num_line_number_entries = nlne;
      sh_flags = (sf1,sf2)
    }
  end;;



(* get_sections
   Reads the section headers and relocations from the file.
   Returns as (section header array, relocation list array). *)

let get_sections h strs ch =
   let sections = Array.create h.h_num_sections bogus_section_header in
   for i = 0 to h.h_num_sections - 1 do
      let sh = (get_section_header ch strs) in
      (* print_section_header sh; *)
      sections.(i) <- sh
   done;
   let relocs = Array.init h.h_num_sections
	 (fun i -> get_relocations sections.(i) ch) in

   (sections, relocs)
;;



(***** Symbols *****)

(* get_coff_symbol
   Reads and returns a COFF symbol. *)

let get_bogus_symbol ch =
  let offset = pos_in ch in
  let s_name = String.create 18 in
  really_input ch s_name 0 18;
  let s_value = -1 in
  let s_section = -1 in
  let s_type = -1 in
  let s_class = -1 in
  let s_aux = -1 in
  { coff_sym_offset = offset;
    coff_sym_name = s_name;
    coff_sym_value = s_value;
    coff_sym_section = s_section;
    coff_sym_type = s_type;
    coff_sym_class = s_class;
    coff_sym_num_aux = s_aux;
  }
;;

let get_coff_symbol ch strs =
  let s_offset = pos_in ch in
  let s_name = get_coff_string ch strs in
  let s_value = get_int ch in
  let s_section = get_short ch in
  let s_type = get_short ch in
  let s_class = get_byte ch in
  let s_aux = get_byte ch in
  { coff_sym_offset = s_offset;
    coff_sym_name = s_name;
    coff_sym_value = s_value;
    coff_sym_section = s_section;
    coff_sym_type = s_type;
    coff_sym_class = s_class;
    coff_sym_num_aux = s_aux;
  }
;;


(* get_symbols
   Returns an array consisting of the symbols in the file. *)

let get_symbols h strs ch =
   let save_pos = (pos_in ch) in
   (seek_in ch h.h_sym_table_offset);

   (* Read each COFF symbol, and place it in the array.
      enter auxiliary entries as bogus entries. *)
   let symbol_array = (Array.create h.h_sym_table_entries bogus_symbol) in
   let i = ref 0 in
   let aux = ref 0 in
   while !i < (Array.length symbol_array) do
     if !aux > 0 then
       begin
	 symbol_array.(!i) <- (get_bogus_symbol ch);
	 aux := !aux - 1;
	 i := !i + 1
       end
     else
       let coff_symbol = get_coff_symbol ch strs in
       symbol_array.(!i) <- coff_symbol;
       i := !i + 1;
       aux := coff_symbol.coff_sym_num_aux;
   done;
   (seek_in ch save_pos);
   symbol_array
;;

let symtype_null = 0
and symclass_external = 2
and symclass_static = 3
and symclass_weakext = 108
and symsection_undef = 0
;;

let is_imported_sym s = s.coff_sym_class = symclass_external;;
let is_exported_sym s = s.coff_sym_class = symclass_weakext;;

(* split_symbols : int -> coff_symbol array ->
 *                  (int,identifier list) dict array
 *
 * Given all of the symbols we produce an array (indexed by section) of
 * dictionaries (indexed by relative position) of lists of symbols
 * (the list should have all symbols at the same position).  This
 * allows us to determine whether a virtual address within a section
 * has a label associated with it.  Note that multiple labels may
 * be associated with an address -- we assume that the labels were
 * placed in the symbol table in the order in which they appeared
 * in the original TAL file.
 *)
let split_symbols num_secs symbols =
  let sec_syms = Array.create num_secs [] in
  for i = 0 to (Array.length symbols) - 1 do
    let s = symbols.(i) in
    let sec = s.coff_sym_section - 1 in
    if sec >= 0 && sec < num_secs then sec_syms.(sec) <- s::sec_syms.(sec)
  done;
  let comp_sym s1 s2 =
    if s1.coff_sym_value = s2.coff_sym_value then
      s1.coff_sym_offset >= s2.coff_sym_offset
    else s1.coff_sym_value <= s2.coff_sym_value in
  for i = 0 to num_secs - 1 do
    sec_syms.(i) <- Sort.list comp_sym sec_syms.(i)
  done;
  (* merge symbols that are the same into a list, preserving order *)
  let sym2id s = id_of_string s.coff_sym_name in
  let rec merge dict syms =
    match syms with
      [] -> dict
    | s1::rest ->
	let rec split lis =
	  match lis with
	    [] -> ([],[])
	  | s2::rest ->
	      if s2.coff_sym_value = s1.coff_sym_value then
		let (same,others) = split rest in
		((sym2id s2)::same,others)
	      else ([],lis) in
	let (same,others) = split rest in
	merge (Dict.insert dict s1.coff_sym_value ((sym2id s1)::same)) others
  in Array.map (merge (Dict.empty compare)) sec_syms
;;

let print_symbols dict =
  Dict.app_dict
    (fun i sl -> Printf.printf "offset %d: " i;
      List.iter	(fun s -> Printf.printf "|%s| " (id_to_string s)) sl;
      Printf.printf "\n") dict
;;

let coff_objfile filename =
  let ch = open_in_bin filename in
  let header = get_header ch in
  let _ = get_optional_header header ch in
  let string_table = read_string_table header ch in
  (*
  for i=0 to (String.length string_table) - 1 do
    if i mod 10 = 0 then Printf.printf "\n%5d: " i;
    Printf.printf "%4s " (Char.escaped (String.get string_table i));
  done;
    *)
  let (section_headers, relocs) = get_sections header string_table ch in
  let symbols = get_symbols header string_table ch in

(*
  Printf.printf "symbol table:\n";
  for i=0 to (Array.length symbols) - 1 do
    let s = symbols.(i) in
    Printf.printf "%d: %s, val=%d, sec=%d, type=%d, class=%d, num_aux=%d\n"
      i s.coff_sym_name s.coff_sym_value s.coff_sym_section
      s.coff_sym_type s.coff_sym_class s.coff_sym_num_aux;
  done;
  for i=0 to (Array.length relocs) - 1 do
    Printf.printf "relocations for section %d\n" i;
    print_relocations relocs.(i) symbols;
  done;
*)

  let sec_symbols = split_symbols header.h_num_sections symbols in
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

  let label_offset =
    Array.fold_left
      (fun d s ->
        Dict.insert d
          (id_of_string s.coff_sym_name)
          (s.coff_sym_section,s.coff_sym_value))
      (Dict.empty id_compare)
      symbols in
  (* seek to a particular section and ensure that the flags are set
   * appropriately. set the cur_<foo> refs. *)

  let seek_section s exn (flag1, flag2) () =
    try
      begin
	cur_section := 0;
	(try
	  while section_headers.(!cur_section).sh_section_name <> s do
	    cur_section := !cur_section + 1
	  done
	with Invalid_argument "Array.get" -> raise exn);
	let cur_sec = section_headers.(!cur_section) in
	if (fst(cur_sec.sh_flags) <> flag2) or
	   (snd(cur_sec.sh_flags) <> flag1) then raise exn else ();
	cur_addr := int32_to_int(cur_sec.sh_virtual_address);
	seek_in ch cur_sec.sh_section_offset;
	cur_section_stop := cur_sec.sh_section_offset + cur_sec.sh_section_size;
	cur_relocations  := relocs.(!cur_section);
	cur_symbols      := sec_symbols.(!cur_section);
	cur_shares       := sec_shares.(!cur_section);
      end
    with _ -> raise exn in

  (* Get all of the labels associated with the position and return
   * as a list.  *)
  let current_labels offset =
    try Dict.lookup (!cur_symbols) ((!cur_addr)+offset)
    with Dict.Absent -> [] in
  (* Do NOT curry this -- state dereference must be delayed *)
  let current_shares () = !cur_shares in

  (* Get the next byte in a section, raising End_of_Section if we go past
   * the end of the current section. *)
  let obj_get_byte () =
    if (pos_in ch) < (!cur_section_stop) then
      (cur_addr := (!cur_addr) + 1; get_byte ch)
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
	if r.coff_rel_pos = (!cur_addr) then
	  begin
	    cur_relocations := rest;
	    Some(id_of_string(symbols.(r.coff_rel_symindex).coff_sym_name))
	  end
	else None in

  (* Sections are numbered 1 ... n , but cur_section goes from 0 .. n-1  *)
  let get_pos () = (!cur_section + 1,!cur_addr) in

  { obj_close = (fun () -> close_in ch);
    obj_seek_code    = seek_section ".text" No_Code_Section shflags_textsection;
    obj_seek_data    = seek_section ".data" No_Data_Section shflags_datasection;
    obj_seek_cyclone = seek_section ".cyc"  No_Cyclone_Section shflags_cyclonesection;
    obj_get_labels   = current_labels;
    obj_get_shared_labels = current_shares;
    obj_get_byte     = obj_get_byte;
    obj_align        = obj_align;
    obj_get_reloc    = get_reloc;
    obj_get_pos      = get_pos;
    obj_label_offset = label_offset
  }
end

