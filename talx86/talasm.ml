(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Talasm
 * Generic TAL assembler, x86 stuff is in Talasmx86
 *)

(* Changes:
   RLS 2/23/99: 
   - Added code for Cyclone.
   - Changed some internal functions for greater modularity. Preparsing code
     is no longer used to fill the symbol list (but it is still used to fill
     the symbol dictionary).

   RLS 11/15/98:
     Added the DEBUG_ASSEMBLER flag seen below. By default it is true for now.
     Added assemble_instr, which checks for instruction length errors when
     DEBUG_ASSEMBLER is true. If you see "Assembler assertion: instruction has
     grown", then there's something wrong with length_of_instr for that
     instruction in talasmx86.ml.

   RLS 11/10/98:
     update_symbol_size now prints an error message if a block has grown.
     If you see "Assembler assertion: block has grown", then there's something
     wrong with length_of_operand in talasmx86.ml.


   To do:
   - Is it better to store addends in relocations and put them in later in the
   linking pass (done now), or to write the addends in the second pass, and
   read them and add the symbol offsets to them in the linking pass? The first
   way stores addends along with the relocations (I thought this was good)
   and is better for ELF "rela" relocations; the second is better for COFF
   relocations and ELF "rel" relocations.
*)

open Numtypes;;
open Identifier;;
open Tal;;
open Objfile;;
open Talasmx86;;
open Talpp;;

let debug_assembler = ref true;;

(***** asm_env *****)

(* make_asm_env
 * Returns a new asm_env. This function SHOULD NOT CHANGE between
 * implementations.
 *)

let make_asm_env schan r_symdict pos_offset r_reloclist sec_num = 
  { a_schan = schan;
    a_r_symdict = r_symdict;
    a_hole_syms = [];
    a_secnum = sec_num;
    a_pos_offset = pos_offset;
    a_r_reloclist = r_reloclist }
;;

(********************************** SYMBOLS **********************************)

(***** Imports and exports *****)

(* make_import_symbols
   Creates an import symbol for each identifier in the imports list.
   Returns: a list of import symbols.
   Side effects: all symbols are inserted into the symbol dict.
     Any special symbols are inserted without identifier numbers. *)

let rec make_import_symbols r_symdict imports =
   match imports with
      [] -> []
    | ((import, _) :: tail) ->
	 let import_symbol = (make_external_symbol import) in
	 (insert_symbol_in_dict r_symdict import_symbol);
	 import_symbol :: (make_import_symbols r_symdict tail)
;;

(* export_symbol
   Side effects:
     - Looks up the symbol with identifier ident, and makes it global. *)

let export_symbol symdict (ident, _) =
   let symbol = (lookup_symbol symdict ident) in
   symbol.sym_scope <- Global
;;

(* update_symbol_pos
   Looks up the symbol with identifier ident in the symbol dictionary,
   and changes its position to newpos.

   Returns:
     - The difference between the real position and the expected
       position. E.g., if expected position is 12 and real position is 10,
       returns 2.
   Side effects:
     - Changes the position of the symbol to newpos. 

   RLS 2/26/99: Now works on a symbol instead of an ident. *)

let update_symbol_pos symbol newpos =
   if symbol.sym_section = 0 then
      ((failwith "update_symbol_pos: symbol is external.");
	 0)
   else
      let pos_offset = symbol.sym_offset - newpos in
      symbol.sym_offset <- newpos;
      pos_offset
;;


(* update_symbol_size
   Looks up the symbol with identifier ident in the symbol dictionary,
   and changes its position to newpos.

   Side effects:
     - Changes the size of the symbol to newsize. 

   RLS 2/26/99: Now works on a symbol instead of an ident. *)

let update_symbol_size symbol newsize =
   (* Prints an error message if block has grown. --RLS 11/10/98*)
   if newsize > symbol.sym_size then
      (Printf.eprintf
	    "Assembler assertion: block has grown.\n\t %s was %d bytes, now is %d bytes.\n"
	    (id_to_string symbol.sym_ident) symbol.sym_size newsize);

   symbol.sym_size <- newsize;
;;

(****************************** CODE SECTIONS ********************************)

(***** Code preparsing *****)

(* length_of_instrs
   Returns: The length of all the instructions in the vector. *)

let rec length_of_instrs instrs =
   let r_length = ref 0 in
   for count = 0 to (Array.length instrs) - 1 do
      try
	 r_length := !r_length + (length_of_instr instrs.(count));
      with
	 Invalid_argument s ->
	    (invalid_arg (Printf.sprintf "instruction %d: %s" count s))
   done;
   !r_length
;;


(* RLS 2/22/99: Split preparse_code into preparse_code and 
     preparse_code_block. *)

(* preparse_code_block
   Preparses a code block.

   Returns: The code block's length.
   Side effects: A symbol for the code block is created in r_symlist and 
     inserted in r_symdict. *)

let preparse_code_block r_symdict r_symlist sec_num offset block = 
   let (ident, conopt, instrs) = block in
   try
      let block_size = (length_of_instrs instrs) in
      let block_symbol =
	 (make_internal_symbol ident sec_num offset block_size) in
      (insert_symbol r_symdict r_symlist block_symbol);
      block_size
   with
      Invalid_argument s ->
	 (invalid_arg (Printf.sprintf
			    "Preparsing error, label %s: %s"
			    (id_to_string ident) s))
;;		  


(* preparse_code
   Preparses each of the code blocks, finding its length, and creating
   a symbol for the code block.

   Returns: startpos + total code length.
   Side effects: A symbol is created in r_symlist and inserted in r_symdict
     for every code block. *)

let preparse_code r_symdict r_symlist sec_num startpos blocks =
   let r_offset = ref startpos in
   for count = 0 to (Array.length blocks) - 1 do
     let block = blocks.(count) in
     let block_size = (preparse_code_block r_symdict r_symlist sec_num 
			     !r_offset block) in
      r_offset := !r_offset + block_size;
   done;
   !r_offset
;;


(* Cyclone *)

(* cyc_preparse_templates
   Preparses each of the templates, finding its length and adding its
   symbols (including its hole symbols) to the symbol list.

   Returns: startpos + the length of all the templates.
   Side effects: All symbols in the templates are added to r_symdict and 
   r_symlist. *)
let cyc_preparse_templates r_symdict sec_num startpos templatev =
   let r_offset = ref startpos in
   let r_symlist = ref [] in
   let blockparse b = 
      let len = (preparse_code_block r_symdict r_symlist sec_num 
		      !r_offset b) in
      let (_, _, instrs) = b in
      let add_hole h offset =
	let hole_symbol = (make_internal_symbol h sec_num offset 0) in
	(insert_symbol r_symdict r_symlist hole_symbol) in
      let check_inst i offset =
	match i with
	| CgHole(_,_,h)          -> add_hole h offset
	| CgHoleJmp(_,(h,_))     -> add_hole h offset
	| CgHoleJcc(_,_,(h,_),_) -> add_hole h offset 
	| _ -> () in
      let rec aux offset instrs =
	match instrs with
	  [] -> ()
	| hd :: tl -> (check_inst hd offset; 
		       aux (offset + (length_of_instr hd)) tl)
      in
      aux (!r_offset) (Array.to_list instrs);
      r_offset := !r_offset + len 
   in
   for count = 0 to (Array.length templatev) - 1 do
      let (i1, tcon, codeblocks) = templatev.(count) in

      (* Add the template symbol. *)
      let template_symbol = (make_internal_symbol i1 sec_num !r_offset 0) in
      (insert_symbol r_symdict r_symlist template_symbol);

      r_offset := !r_offset + 4;	(* Add space for template length *)
      (List.map blockparse codeblocks);
   done;
   !r_offset
;;

(* End Cyclone *)



(***** Code assembly *****)

(* assemble_instr
   Assemble a single instruction. If in debug mode, check to see that
   the real length does not exceed the predicted length. *)

let assemble_instr env instr =
   if !debug_assembler then
      begin
	 let start_pos = (Stringchan.get_mark env.a_schan) in
	 let result = (write_instr env instr) in
	 let actual_length = ((Stringchan.get_mark env.a_schan) - start_pos) in
	 let predicted_length = (length_of_instr instr) in
	 if actual_length > predicted_length then
	    begin
	       Printf.printf "Assembler assertion: instruction has grown.\n";
	       (Talpp.print_instruction Format.std_formatter
		     {style = MASM; kinds = false; cons = false; expand_abbrevs = true;
		      expand_pr = false; detail = FullDetail } instr);
	       (Printf.printf "\n\tPredicted %d bytes, actually %d bytes.\n"
		     predicted_length actual_length);
	    end
      end
   else
      (write_instr env instr)
;;



(* assemble_code_block
   Assembles a code block, updating the block's reference in the symbol
   dict to point to its actual start position.

   Preconditions:
     - schan must be a string channel at least as long as we'll need.
   Side effects:
     - The block's symbol in symdict is updated to its new position.
     - Some instructions (namely, Cyclone hole macros) add symbols to
         env.a_symdict.
     - Relocations are inserted into r_reloclist.
     - The assembled code is written into schan. 
   Returns: 
     - The symbol for the code block. *)

let assemble_code_block env block = 
   let (ident, _, instrs) = block in
   let realpos = (Stringchan.get_mark env.a_schan) in
   let block_sym = (lookup_symbol !(env.a_r_symdict) ident) in
   env.a_pos_offset <- (update_symbol_pos block_sym realpos);
   for i = 0 to (Array.length instrs) - 1 do
      try
	 assemble_instr env instrs.(i);
      with
	 Invalid_argument s ->
	    invalid_arg (Printf.sprintf
			      "Assembly error, label %s, instr %d: %s"
			      (id_to_string ident) i s)
   done;
   (update_symbol_size block_sym 
	 ((Stringchan.get_mark env.a_schan) - realpos));
   block_sym
;;

   

(* RLS 2/26/99: eliminated assemble_code. *)


(* make_code_section
   Assembles the code from a tal_code, and returns a new code section.

   Preconditions: The code must already have been preparsed, and a symbol
       must exist for every identifier in the section.

   Returns:  A ".text" section with data, relocs, and symbol fields set.
   Side effects:  - symbols in symdict are updated to be in their proper
       positions. *)

let make_code_section sec_num r_symdict code_length imp =
   let r_relocs = ref [] in
   let schan = (Stringchan.create code_length) in
   let env = (make_asm_env schan r_symdict 0 r_relocs sec_num) in
   let code_syms = (Array.map (assemble_code_block env) imp.code_blocks) in
   let syms = Array.to_list code_syms in
   if env.a_hole_syms <> [] 
   then failwith "Holes found outside of template." else
   (make_section ".text" schan
	 (List.rev !r_relocs)		(* Need to reverse reloc list. *)
	 (Array.to_list code_syms))
;;



(* Cyclone *)

(* cyc_make_template_section
   Assembles the code in cyclone templates, and returns a 
   template section.

   Preconditions: The code must already have been preparsed, and a symbol
       must exist for every identifier in the section.

   Returns:  A ".cyc" section with data, relocs, and symbol fields set.
   Side effects:  
     - symbols in symdict are updated to be in their proper
       positions. 
     - hole symbols are added to symdict as they are found, but they are
       not put into the section symbol list. *)

let cyc_make_template_section sec_num r_symdict max_length imp =
   let r_relocs = ref [] in
   let schan = (Stringchan.create max_length) in
   let env = (make_asm_env schan r_symdict 0 r_relocs sec_num) in
   let tmpl_syms = ref [] in

   for count = 0 to (Array.length imp.templates) - 1 do
      let (i1, tcon, codeblocks) = imp.templates.(count) in

      let tmpl_start = (Stringchan.get_mark schan) in
      (* Update template symbol. *)
      let template_symbol = (lookup_symbol !r_symdict i1) in
      (update_symbol_pos template_symbol tmpl_start);
      (Stringchan.put_4bytes schan 0); (* Skip template length dword *)
      (* If multiple labels occur at the same location in the object file
	 the order they occur here determines their order in the symbol table.
	 The symbol talbe order is used when zipping together a .to file and 
	 an object file.  So despite whatever you may believe, the order of 
	 symbols in tmpl_syms is important!!! *)
      tmpl_syms := !tmpl_syms@(template_symbol :: 
			       (List.map (assemble_code_block env) codeblocks));
      let tmpl_end = (Stringchan.get_mark schan) in

      tmpl_syms := !tmpl_syms @ env.a_hole_syms;
      env.a_hole_syms <- [];
      (* Write template length. *)
      let tmpl_len = tmpl_end - tmpl_start - 4 in
      (* FMS: For profiling we output the length of templates as we assemble 
	 them *)
      Printf.printf "Template %s(%d bytes)\n" (id_to_string template_symbol.sym_ident) tmpl_len;
      (Stringchan.set_mark schan tmpl_start);
      (Stringchan.put_4bytes schan tmpl_len);
      (Stringchan.set_mark schan tmpl_end);
   done;
   
   (make_section ".cyc" schan
	 (List.rev !r_relocs)		(* Need to reverse reloc list. *)
	 (!tmpl_syms))
;;
      
(* End Cyclone *)


(****************************** DATA SECTIONS  *******************************)

(***** Data preparsing *****)

(* At position pos how much padding should we insert to achieve 
   alignment align *)
let padding align pos =
  if align = 0 then 0 else 
  let t = pos mod align in
  if t = 0 then 0 else (align - t)
;;

(* length_of_data_items
   Returns the sum of the lengths of all of the data items in the list. *)

let rec length_of_data_items ditems =
   match ditems with
      [] -> 0            
    | (ditem :: tail) -> 
	(length_of_data_item ditem) + (length_of_data_items tail)
;;

(* preparse_data
   Preparses each of the data blocks, finding its length, and creating
   a symbol for it.

   Returns: total data length.
   Side effects: A symbol is created in r_symlist and inserted into r_symdict
     for each data symbol. *)

let preparse_data r_symdict r_symlist sec_num startpos blocks =
   let r_offset = ref startpos in
   for count = 0 to (Array.length blocks) - 1 do
     let (ident, align , _, (ditems, _)) = blocks.(count) in
     let align = int32_to_int align in
     let data_size = (length_of_data_items ditems) in
     r_offset := !r_offset + (padding align !r_offset);
     let data_symbol = (make_internal_symbol ident sec_num !r_offset
			  data_size) in
     (insert_symbol r_symdict r_symlist data_symbol);
     r_offset := !r_offset + data_size;
   done;
   !r_offset				(* Return total data length. *)
;;

(***** Data assembly *****)

(* assemble_data
   Assemble each data block.

   Preconditions:
     - schan must be a string at least as long as we'll need.
   Side effects:
     - Relocations are inserted into r_reloclist.
     - The assembled data is written into schan. *)

let assemble_data schan r_reloclist sec_num r_symdict blocks =
   let env = (make_asm_env schan r_symdict 0 r_reloclist sec_num) in
   for count = 0 to (Array.length blocks) - 1 do
     let (ident,align,_,(data,_)) = blocks.(count) in
     let align = int32_to_int align in
     for i = 1 to (padding align (Stringchan.get_mark schan)) do
       Stringchan.put_byte schan 0;
     done;
     List.iter (write_data_item env) data
   done
;;

(* make_data_section
   Takes a TAL implementation, and returns a data section.

   Preconditions: The data section must already have been preparsed,
      and symdict must contain its symbols.

   Returns: A ".data" section with data, relocs, and symbol fields. *)

let make_data_section sec_num symdict data_symbols data_length imp =
   let r_data_relocs = ref [] in
   let data_chan = (Stringchan.create data_length) in
   (assemble_data data_chan r_data_relocs sec_num symdict imp.data_blocks);
   (make_section ".data" data_chan
	 (List.rev !r_data_relocs)
	 (List.rev data_symbols))
;;

(******************************** LINKING *********************************)

(***** Linker *****)

(* is_reloc_internal
   Returns TRUE if the relocation is internal to the section. *)

let is_reloc_internal sec_num reloc =
   ((reloc.rel_symbol.sym_section = sec_num) && (not reloc.rel_force_public))
;;

(* reloc_dest
   Returns the destination of a relocation, as either an absolute address
   or an offset from a given position. *)

let reloc_dest pos reloc =
   match reloc.rel_relativity with
      Absolute ->
 	(reloc.rel_symbol.sym_offset + (int32_to_int reloc.rel_addend))
    | Relative ->
 	((reloc.rel_symbol.sym_offset + (int32_to_int reloc.rel_addend)) - pos)
;;

(* put_reloc_dest
   Side effects:
     - Writes the relocation destination at the given position. *)

let put_reloc_dest data pos reloc =
   (Stringchan.set_mark data pos);
   match reloc.rel_scale with
      Byte1 -> (Stringchan.put_byte data (reloc_dest (pos + 1) reloc))
    | Byte2 -> (Stringchan.put_2bytes data (reloc_dest (pos + 2) reloc))
    | Byte4 -> (Stringchan.put_4bytes data (reloc_dest (pos + 4) reloc))
    | _ -> failwith "Linker: put_reloc_dest: Bad relocation scale."
;;

(* link_relocs
   Links all section-internal relocations in the given list of relocations.
   Returns:
     - A list of relocations with the linked internal relocations removed.
   Side effects:
     - Identifiers in the section's data that used internal relocations now
       have the proper addresses. *)

let rec link_relocs sec_num data relocs =
   match relocs with
      [] -> []
    | (reloc :: tail) ->
	 if (is_reloc_internal sec_num reloc) then
	    begin
	       (put_reloc_dest data reloc.rel_pos reloc);
	       (link_relocs sec_num data tail)(* Delete the used relocation. *)
	    end
	 else
	    (reloc :: (link_relocs sec_num data tail))
;;

(* link_sections
   Links the internal relocations in each section.
   Side effects:
     - All internal relocations are removed from each section's relocation
       list.
     - Identifiers in the section's data that used internal relocations are
       linked to the proper addresses. *)

let rec link_sections sec_num secs =
   match secs with
      [] -> ()
    | (sec :: tail) ->
	 let savemark = (Stringchan.get_mark sec.s_data) in
	 sec.s_relocs <- (link_relocs sec_num sec.s_data sec.s_relocs);
	 (Stringchan.set_mark sec.s_data savemark);
	 (link_sections (sec_num + 1) tail)
;;

(* prune_imports
   After internal linking, constructs a new import list based on the 
   remaining relocations.
   Side effects:
     - replaces old import list with newly generated one *)
let prune_imports objfile =
  let rec prune_section seclist symlist =
    match seclist with
      [] -> symlist
    | (sec::t) -> 
	(* segregate each import in the relocations *)
	let symlist' =
	(* for each relocation in this section ... *)
	  List.fold_right
	    (fun rel syml -> 
	      (* add symbol to import list *)
	         (* if not present *)
	      if (not (List.exists 
			 (fun sym -> sym = rel.rel_symbol) 
			 syml)) &&
                 (* symbol is external *)
	         (rel.rel_symbol.sym_section = 0)
	      then
		rel.rel_symbol::syml
	      else
		syml) sec.s_relocs symlist in
	(* continue for remaining sections *)
	prune_section t symlist' in

  let o_symbols = objfile.o_import_symbols in
  objfile.o_import_symbols <- prune_section objfile.o_secs [];
  (* debugging code -- make sure the generated list is a subset
     of the declared import list *)
  let rec sublist sl l =
    match (sl,l) with
      ([],_) -> true
    | (_,[]) -> false
    | (sh::st,l) ->
	let found = ref false in
	let l' = List.fold_right
	    (fun sym syml ->
	      if sh = sym then 
		(found := true; syml)
	      else sym::syml) l [] in
	if !found then
	  sublist st l'
	else
	  false in
  if (sublist objfile.o_import_symbols o_symbols) then
    ()
  else
    (Printf.printf("Error: declared list does not contain generated list\n");
    let print_sym { sym_ident = ident } =
      Printf.printf "  sym %s\n" (id_to_string ident) in
    Printf.printf ("Generated list:\n");
    List.iter print_sym objfile.o_import_symbols;
    Printf.printf ("Declared list:\n");
    List.iter print_sym o_symbols;
    objfile.o_import_symbols <- o_symbols
)
;;

(* link_file
   Performs internal linking for every section in the file.
   Side effects: 
     - Internal relocations are linked and removed. 
     - All imports that are not actually referred to
       are removed from the import list *)

let link_file objfile =
  (link_sections 1 objfile.o_secs;
   prune_imports objfile)
;;

(***************************** ENTRY FUNCTION *****************************)

(***** Assembly *****)

(* assemble
   Assembles a Tal implementation into an object file.
   Returns: The assembled object file. 
   Updated for cyclone templates. All variables pertaining to cyclone are
   marked "cyc". All cyclone templates are put in the code section. *)

let assemble imp (imports_intt, exports_intt) = 
   let r_symdict = ref (empty_symbol_table()) in
   let r_code_symbols = ref [] in
   let r_data_symbols = ref [] in
					(* Preparse & gather symbols. *)
   let import_labels = special_labels in
   (* Cyclone *)
   let import_labels = import_labels @ Cyclone.cyclone_labels in
   (* End Cyclone *)

   let import_symbols = (make_import_symbols r_symdict
			      (import_labels @ imports_intt.it_vals)) in
   let code_length = (preparse_code r_symdict r_code_symbols 1 0
			   imp.code_blocks) in
   let data_length = (preparse_data r_symdict r_data_symbols 2 0
			   imp.data_blocks) in
   (* Cyclone *)
   let cyc_length = (cyc_preparse_templates r_symdict 3 0
			  imp.templates) in

   let cyc_section = (cyc_make_template_section 3 r_symdict
			   cyc_length imp) in
   (* End Cyclone *)

					(* Declare export symbols public. *)
   (List.iter (export_symbol !r_symdict) exports_intt.it_vals);

					(* Assemble code and data. *)
   let code_section = (make_code_section 1 r_symdict 
			    code_length imp) in

   let data_section = (make_data_section 2 r_symdict !r_data_symbols
			    data_length imp) in

					(* Create an object file structure. *)
   let sections = [code_section; data_section] in

   (* Cyclone *)
   let sections = sections @ [cyc_section] in
   (* End Cyclone *)

   let objfile = (make_objfile sections import_symbols !r_symdict) in
   (link_file objfile);			(* Link all local symbols. *)

   objfile
;;

(* EOF: talasm.ml *)
