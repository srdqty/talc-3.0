(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* dasm.ml
 *
 * Disassembles an object file and associated .to file to produce a
 * TAL implementation.
 *)

let pp i = 
  print_string ("Got here " ^ (string_of_int i) ^"\n");
  flush stdout;
  ()
;;


open Tal;;
open Talbin;;
open Talbinin;;
open Numtypes;;
open Disobjfile;;
open Utilities;;

let debug f = 
  let fmt = Format.std_formatter in 
  let o = Talpp.std_options in 
  Format.pp_open_hvbox fmt 0;
  f fmt o;
  Format.pp_print_newline fmt ();
  Format.pp_print_flush fmt (); 
  () 
;;


module type dasm = sig
   val disassemble : bool -> string -> string -> Tal.tal_pre_mod
end 

module DasmFunc (X: sig 
   type in_channel
   val input_char : in_channel -> char
   val input_byte : in_channel -> int
   val pos_in : in_channel -> int 
   val seek_in : in_channel -> int -> unit
   val open_in_bin : string -> in_channel
   val close_in : in_channel -> unit
   val really_input : in_channel -> string -> int -> int -> unit
end ) : dasm = struct

open X
module TalbininX = Talbinin.TalbininFunc(X)
module Discoff = Discoff.DiscoffFunc(X)
module Diself = Diself.DiselfFunc(X)

type binformat = COFF | ELF;;

let objformat =
  match Sys.os_type with
    "Unix" -> ELF
  | "Win32" -> COFF
  | _ -> Printf.eprintf "Unknown operating system\n"; exit 1

let fail objf s = 
  let (sect,addr) = objf.obj_get_pos() in
  Printf.eprintf "dasm (sect=%d, addr=0x%x): %s\n"
    sect addr s;
  raise Gcdfec.Exit;
;;

let get_code_blocks objf code_annots init_labs = 
  (* the annotated code blocks -- initialized with the types only *)
  let code_blocks = 
    let bogus_id = Identifier.id_new "\000bogus" in
    Array.init (Array.length code_annots) 
      (fun i -> (bogus_id,fst(code_annots.(i)), Array.of_list [])) in
  let merge_inst_annots = merge_inst_annots objf in
  (* get the code blocks and merge with the annotations *)

  (* gets a list of intructions and the next list of labels *)
  let rec get_rest_instrs () = 
    match objf.obj_get_labels 0 with
      [] -> (try get_instrs () with End_of_Section -> ([],[]))
    | labs -> ([],List.rev(labs)) 
  and get_instrs () = 
    let instr = Disasmx86.get_instr objf in
    let (instrs,labs) = get_rest_instrs() in
    (instr::instrs,labs)
  in
  let labs = ref init_labs in
  let rec merge_inst_annots' annots raw_insts =
    try
      merge_inst_annots annots raw_insts 
    with Talbin.Found_Hole (annots,insts) ->
      let (raw_instrs,next_labs) = get_instrs() in
      (match !labs with
	(* XXX - we should check that the hole label matches that in the 
	   first annotation in annots. *)
	[holeLab] -> ()
      |	[] -> fail objf "No hole label here."
      |	_ ->
	  Printf.eprintf "Labels = ";
	  List.map (fun x -> Printf.eprintf "%s " (Identifier.id_to_string x)) !labs;
	  Printf.eprintf "\n";
	  flush stderr;
	  fail objf "Too many hole labels here.");
      labs := next_labs;
      (insts @ (merge_inst_annots' annots raw_instrs))
  in
  for i = 0 to (Array.length code_annots) - 1 do
    let do_code_block lab raw_instrs =
      let (_,copt,_) = code_blocks.(i) in
      let instrs = merge_inst_annots' (snd code_annots.(i)) raw_instrs in
      code_blocks.(i) <- (lab,copt,Array.of_list instrs);
    in
    begin match !labs with
    | [] -> fail objf "More annotations than code blocks!"
    | [lab] ->
      (* one label -- should be followed by instructions which we suck
       * in, merge with the annotations, and then bang into the code_blocks
       * array.  As a side-effect of reading the instructions, we get the
       * next list of labels and loop around.  Note that the labels are
       * sucked in in what appears to be reverse order from the original
       * TAL file. *)
	let (raw_instrs,next_labs) = get_instrs() in
	labs := next_labs;
	do_code_block lab raw_instrs;
	()
    | lab::rest ->
	(* we have more than one label -- so the instructions are empty.
	 * However, the annotations may not be because we may have virtual
	 * instructions, so we have to merge them and then loop around with
	 * the rest of the labels. *)
	labs := rest;
	do_code_block lab [];
	()
    end;
(*    Format.open_vbox 0;
    Talpp.print_code_block 
      Format.std_formatter Talpp.std_options code_blocks.(i);
    Format.close_box ();
    Format.print_flush ();
    flush stdout;
*)
  done;
  (code_blocks, !labs)
;;

let get_code_section  code_annots objf =
  (* seek the code in the object file *)
  objf.obj_seek_code();
  let init_labs = 
    (match List.rev (objf.obj_get_labels 0) with
      [] -> fail objf "no initial text labels!"
    | t1::rest -> 
	if (Identifier.id_to_string t1) = ".text" then rest
	else fail objf "expecting .text label!") in
  let (code_blocks,labs) = get_code_blocks objf code_annots init_labs in
    match labs with
    | [] -> code_blocks
    | _ -> fail objf "more labels, but no annotations."
;;
  
let get_templates template_annots objf =
  try 
    objf.obj_seek_cyclone ();
    let init_labs = 
      (match List.rev (objf.obj_get_labels 0) with
	[] -> fail objf "no initial text labels!"
      | t1::rest -> 
	  if (Identifier.id_to_string t1) = ".cyc" then rest
	  else fail objf "expecting .cyc label!") in
    let templates = 
      let bogus_id = Identifier.id_new "\000bogus" in
      Array.init (Array.length template_annots) 
	(fun i -> (bogus_id,fst(template_annots.(i)), 
		   [])) in
    let labs = ref init_labs in
    for i = 0 to (Array.length template_annots) - 1 do
      match !labs with
	[] -> fail objf "too few labels, no template start."
      | [templ_start] ->
(*	  Printf.printf "templ_start = %s\n" (Identifier.id_to_string templ_start);
	  flush stdout; *)
	  let (con,annots) = template_annots.(i) in
	  (* We output one word here when assembling to store the template
	     length. Not sure why?  Still when reading this in we should skip
	     4 bytes here. *)
	  objf.Disobjfile.obj_get_byte ();
	  objf.Disobjfile.obj_get_byte ();
	  objf.Disobjfile.obj_get_byte ();
	  objf.Disobjfile.obj_get_byte ();
	  let init_labs = List.rev (objf.obj_get_labels 0) in
	  let (cb,rest) = get_code_blocks objf annots init_labs in
	  templates.(i) <- (templ_start,con,Array.to_list cb);
	  labs := rest
      |	_ -> fail objf "too many labels, at template start."
    done;
    (match !labs with
      [] -> templates
    | _ -> 
	List.iter (fun x -> Printf.printf " %s\n" (Identifier.id_to_string x)) !labs;
	flush stdout;
	fail objf "extra labels in Cyclone section")
  with Disobjfile.No_Cyclone_Section -> (Array.of_list [])
      ;;

let get_data_blocks data_annots objf = 
  let get_data a = (* Given a data annotation read in the data. *)
    match a with
      An_dlabel c -> 
	(match objf.obj_get_reloc () with
	  None -> fail objf "expecting label as data!"
	|	Some lab -> 
	    let b0 = objf.obj_get_byte() in
	    let b1 = objf.obj_get_byte() in
	    let b2 = objf.obj_get_byte() in
	    let b3 = objf.obj_get_byte() in
	    let v = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
	    if v <> 0 then fail objf "relocation adjustment";
	    Dlabel(lab,c))
    | An_dbytes (len) ->
	if len < 0 then fail objf "An_dbytes: size is less than 0"
	else 
	  let s = String.create len in
	  for i = 0 to len - 1 do
	    String.set s i (Char.chr (objf.obj_get_byte()))
	  done; Dbytes (s)
    | An_d2bytes ->
	let lo = objf.obj_get_byte() in
	let hi = objf.obj_get_byte() in
	let v = int_to_int32((hi lsl 8) lor lo) in
	D2bytes(int32_to_int16 v)
    | An_d4bytes c ->
	let b0 = objf.obj_get_byte() in
	let b1 = objf.obj_get_byte() in
	let b2 = objf.obj_get_byte() in
	let b3 = objf.obj_get_byte() in
	let v = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
	D4bytes(int_to_int32 v,c)
    | An_dfloat32 ->
	let b = String.create 4 in
	for i=0 to 3 do
	  b.[i] <- char_of_int (objf.obj_get_byte ());
	done;
	Dfloat32 (bytes_to_f32 b)
    | An_dfloat64 ->
	let b = String.create 8 in
	for i = 0 to 7 do
	  b.[i] <- char_of_int (objf.obj_get_byte ());
	done;
	Dfloat64 (bytes_to_f64 b)
    | An_djunk -> 
	let b0 = objf.obj_get_byte() in
	let b1 = objf.obj_get_byte() in
	let b2 = objf.obj_get_byte() in
	let b3 = objf.obj_get_byte() in
	let v = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
	if v <> 0 then fail objf "data supposed to be junk is non-zero";
	Djunk
    | An_drep ri -> 
	       (* First four bytes describe the size *)
	let b0 = objf.obj_get_byte() in
	let b1 = objf.obj_get_byte() in
	let b2 = objf.obj_get_byte() in
	let b3 = objf.obj_get_byte() in
	let len = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
	let str = String.create len in 	       
	for i = 0 to len - 1 do 
	  String.set str i (Char.chr (objf.obj_get_byte()))
	done;
	let strchan = Stringchan.from_string str in 
	Drep 
	  ((match ri with 
	    An_con -> RCon (Talbinin.Str.read_in_con strchan)
	  | An_kind -> RKind (Talbinin.Str.read_in_kind strchan)
	  | An_label -> RLabel (Talbinin.Str.read_in_label strchan)),
	   ref (Some (str)))
    | An_dup -> Dup
    | An_ddown -> Ddown in
  let data_blocks = 
    Array.make (Array.length data_annots)
      (Identifier.id_new "\000bogusd",i32_4,None,([],[])) in
  let set_data_block i lab =
    let (align,copt,(annots,cs)) = data_annots.(i) in
    data_blocks.(i) <- (lab,align,copt,(List.map get_data annots,cs)) in
  let rec set_data_blocks i labs = 
    match labs with
      [] -> 
	if i <> (Array.length data_blocks) then
	  fail objf "Too few labels in data section."
	else ()
    | [lab] -> 
	set_data_block i lab; 
	if (i+1) < Array.length data_annots then 
	  (let (align,_,_) = data_annots.(i+1) in
	  objf.obj_align (int32_to_int align);
	  set_data_blocks (i+1) (List.rev (objf.obj_get_labels 0)))
    | lab :: rest ->
	set_data_block i lab;
	set_data_blocks (i+1) rest
  in
  objf.obj_seek_data();
  let init_labs = 
    (match List.rev (objf.obj_get_labels 0) with
      [] -> fail objf "no initial data labels!"
    | t1::rest -> 
	if (Identifier.id_to_string t1) = ".data" then rest
	else fail objf "expecting .data label!") in
(*    Printf.printf "initial data labels:\n";
    List.iter (fun x -> Printf.printf "%s\n" (Identifier.id_to_string x)) 
    init_labs;
    Printf.printf "--------------------\n"; *)

  set_data_blocks 0 init_labs;
  data_blocks
;;

let disassemble disassem_internals tofile objfile =
  let tofile_ch = open_in_bin tofile in
  Gcdfec.reset_fe "dasm";
  (* read the .to file *)
  let tal_info = TalbininX.read_tal_info disassem_internals tofile_ch in
  let code_annots = tal_info.ti_code_annots in
  let data_annots = tal_info.ti_data_annots in
  let template_annots = tal_info.ti_template_annots in 
  close_in tofile_ch;
  if disassem_internals then
    begin
      (* open the object file and suck in headers and so forth *)
      let objf = 
	match objformat with
	  COFF -> Discoff.coff_objfile objfile
	| ELF -> Diself.elf_objfile objfile
      in
      (* get the code blocks *)
      let code_blocks = get_code_section code_annots objf in
      (* get the data blocks *)
      let data_blocks = get_data_blocks data_annots objf in
      (* get the template blocks *)
      let templates = get_templates template_annots objf in
      objf.obj_close();
(*      let impl = *)
      { import_refs = tal_info.ti_imports; 
	export_refs = tal_info.ti_exports; 
	pre_imp     = { imp_kindabbrevs = tal_info.ti_kindabbrevs; (* LX *)
			imp_abbrevs     = tal_info.ti_abbrevs;
			con_blocks      = tal_info.ti_con_blocks; 
			code_blocks     = code_blocks;
			data_blocks     = data_blocks; 
			templates       = templates; };
      } 
(*      in

      Talout.write_pre_mod "foo" "temp" impl;
      impl *)

    end
  else 
    { import_refs = tal_info.ti_imports;
      export_refs = tal_info.ti_exports;
      pre_imp     = { imp_abbrevs     = [||];
		      imp_kindabbrevs = [||]; (* LX *)
		      con_blocks      = [||];
		      code_blocks     = [||];
		      data_blocks     = [||];
		      templates       = [||] } ;
    } 
;;

end (* DasmFunc *)

module Chan = DasmFunc(Pervasives)
module Str = DasmFunc (Stringchan)
